# ───────────────────────────────────────────────────────────────
# Packages
# ───────────────────────────────────────────────────────────────
library(shiny)
library(shinythemes)
library(DBI)
library(odbc)
library(pool)
library(dplyr)
library(stringr)
library(lubridate)
library(DT)
library(tidyr)
library(ggplot2)
library(readr)    # only for write_csv in downloads (no reading from CSV files)
options(scipen = 999)  # bias against scientific notation

# ───────────────────────────────────────────────────────────────
# DB Connection Pool (Azure SQL via ODBC Driver 18)
# ───────────────────────────────────────────────────────────────

DB_HOST <- Sys.getenv("SERVER")  # <--- Azure SQL server FQDN
DB_PORT <- 1433
DB_NAME <- Sys.getenv("DATABASE")                 # database name
DB_USER <- Sys.getenv("AZURE_SQL_UID")
DB_PWD  <- Sys.getenv("AZURE_SQL_PWD")  
DB_SCHEMA <- Sys.getenv("SCHEMA_NAME")

if (Sys.getenv("R_CONFIG_ACTIVE") == "connect_cloud") {
  pool <- tryCatch({
    pool::dbPool(
      drv = odbc::odbc(),
      .connection_string = paste0(
        "Driver=FreeTDS;",               # must match odbcinst.ini entry on the host
        "TDS_Version=7.4;",              # try 7.4 first; fallback to 7.3 if needed
        "Server=", DB_HOST, ";",
        "Port=", DB_PORT, ";",
        "Database=", DB_NAME, ";",
        "Uid=", DB_USER, ";",
        "Pwd=", DB_PWD, ";",
        "Encrypt=yes;",
        "TrustServerCertificate=no;",
        "Connection Timeout=30;"
      )
    )
  }, error = function(e) {
    message("FreeTDS connect failed: ", conditionMessage(e))
    NULL
  })
} else {
  pool <- pool::dbPool(
    drv      = odbc::odbc(),
    Driver   = "ODBC Driver 18 for SQL Server",
    Server   = DB_HOST,
    Database = DB_NAME,
    UID      = DB_USER,
    PWD      = DB_PWD,  # set in .Renviron or hosting secret
    Encrypt  = "yes",
    TrustServerCertificate = "no",
    Timeout  = 30
  )
  
}


# ---------------------------------------------------------------
# 1) Create pool (may be NULL on Posit Connect at startup)
# 

create_pool <- function() {
  tryCatch({
    pool::dbPool(
      drv = odbc::odbc(),
      .connection_string = paste0(
        "Driver=FreeTDS;",
        "TDS_Version=7.4;",
        "Server=", DB_HOST, ";",
        "Port=", DB_PORT, ";",
        "Database=", DB_NAME, ";",
        "Uid=", DB_USER, ";",
        "Pwd=", DB_PWD, ";",
        "Encrypt=yes;",
        "TrustServerCertificate=no;",
        "Connection Timeout=30;"
      )
    )
  }, error = function(e) {
    message("❌ create_pool() failed: ", conditionMessage(e))
    NULL
  })
}


# ---------------------------------------------------------------
# 2) Reopen the pool if needed
# ---------------------------------------------------------------

reopen_pool <- function(wait = 2, tries = 3) {
  message("⚠️ Attempting to reopen SQL pool...")
  
  for (i in seq_len(tries)) {
    message("   → Attempt ", i, " of ", tries)
    
    new_pool <- create_pool()
    
    if (!is.null(new_pool)) {
      ok <- tryCatch({
        DBI::dbGetQuery(new_pool, "SELECT 1")
        TRUE
      }, error = function(e) FALSE)
      
      if (ok) {
        message("✅ SQL pool reopened successfully.")
        return(new_pool)
      }
    }
    
    Sys.sleep(wait)
  }
  
  message("❌ Failed to reopen SQL pool after retries.")
  return(NULL)
}


# ---------------------------------------------------------------
# 3) Always use this to get a valid pool
# ---------------------------------------------------------------

get_pool <- function() {
  # pool exists and is valid?
  if (!is.null(pool)) {
    valid <- FALSE
    try(valid <- DBI::dbIsValid(pool), silent = TRUE)
    if (valid) return(pool)
  }
  
  # otherwise reconnect
  pool <<- reopen_pool()
  return(pool)
}


onStop(function() {
  pool::poolClose(pool)
})

# Helper: NULL-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ───────────────────────────────────────────────────────────────
# Helpers
# ───────────────────────────────────────────────────────────────

# Quote [schema].[table] safely for SQL Server via DBI
# Safe schema/table quoting helper

qt <- function(tbl) {
  p <- get_pool()
  
  # If still no pool, fallback
  if (is.null(p)) return(paste0("[", DB_SCHEMA, "].[", tbl, "]"))
  
  ok <- FALSE
  try(ok <- DBI::dbIsValid(p), silent = TRUE)
  if (!ok) return(paste0("[", DB_SCHEMA, "].[", tbl, "]"))
  
  as.character(DBI::dbQuoteIdentifier(
    p,
    DBI::Id(schema = DB_SCHEMA, table = tbl)
  ))
}


# tiny paste helper used below
`%+%` <- function(a,b) paste0(a,b)


new_id <- function() paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1000:9999, 1))
to_month_key <- function(date) format(as.Date(date), "%Y-%m")
fmt_cur <- function(x, currency = "₹") {
  x <- suppressWarnings(as.numeric(x))
  paste0(currency, format(round(x, 2), big.mark = ",", nsmall = 2, trim = TRUE, scientific = FALSE))
}

# CRUD – Users
# User existence
db_user_exists <- function(username) {
  p <- get_pool()
  if (is.null(p)) return(FALSE)
  
  u_tbl <- qt("users")
  n <- DBI::dbGetQuery(pool,
                       sprintf("SELECT COUNT(*) AS n FROM %s WHERE LOWER(username) = LOWER(?)", u_tbl),
                       params = list(username)
  )$n
  as.integer(n) > 0
}

# Create user
db_create_user <- function(username) {
  u_tbl <- qt("users")
  DBI::dbExecute(pool,
                 sprintf("INSERT INTO %s (username, created_at) VALUES (?, SYSUTCDATETIME())", u_tbl),
                 params = list(username)
  )
}

# Insert income
db_insert_income <- function(id, username, month, amount, source) {
  i_tbl <- qt("incomes")
  DBI::dbExecute(pool,
                 sprintf("INSERT INTO %s (id, username, month, income_amount, source, created_at)
             VALUES (?, ?, ?, ?, ?, SYSUTCDATETIME())", i_tbl),
                 params = list(id, username, month, as.numeric(amount), source)
  )
}

# Insert expense
db_insert_expense <- function(id, username, date, month, category, description, amount) {
  e_tbl <- qt("expenses")
  DBI::dbExecute(pool,
                 sprintf("INSERT INTO %s (id, username, [date], month, category, description, expense_amount, created_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, SYSUTCDATETIME())", e_tbl),
                 params = list(id, username, as.Date(date), month, category, description, as.numeric(amount))
  )
}

# Distinct months for a user
db_user_months <- function(username) {
  i_tbl <- qt("incomes"); e_tbl <- qt("expenses")
  DBI::dbGetQuery(pool, sprintf("
    WITH m AS (
      SELECT month FROM %s WHERE username = ?
      UNION
      SELECT month FROM %s WHERE username = ?
    )
    SELECT DISTINCT month FROM m ORDER BY month
  ", i_tbl, e_tbl), params = list(username, username))$month
}

# Read incomes
db_read_incomes <- function(username = NULL, month = NULL) {
  i_tbl <- qt("incomes")
  if (is.null(username) && is.null(month)) {
    return(DBI::dbGetQuery(pool, sprintf("SELECT id, username, month, income_amount, source, created_at FROM %s", i_tbl)))
  }
  if (!is.null(username) && is.null(month)) {
    return(DBI::dbGetQuery(pool, sprintf("
      SELECT id, username, month, income_amount, source, created_at
        FROM %s WHERE username = ? ORDER BY created_at DESC", i_tbl), params = list(username)))
  }
  if (!is.null(username) && !is.null(month)) {
    return(DBI::dbGetQuery(pool, sprintf("
      SELECT id, username, month, income_amount, source, created_at
        FROM %s WHERE username = ? AND month = ? ORDER BY created_at DESC", i_tbl), params = list(username, month)))
  }
  if (is.null(username) && !is.null(month)) {
    return(DBI::dbGetQuery(pool, sprintf("
      SELECT id, username, month, income_amount, source, created_at
        FROM %s WHERE month = ?", i_tbl), params = list(month)))
  }
}

# Read expenses
db_read_expenses <- function(username = NULL, month = NULL) {
  e_tbl <- qt("expenses")
  base <- sprintf("
    SELECT id, username, CONVERT(VARCHAR(10), [date], 23) AS date,
           month, category, description, expense_amount, created_at
      FROM %s", e_tbl)
  if (is.null(username) && is.null(month)) return(DBI::dbGetQuery(pool, base))
  if (!is.null(username) && is.null(month))
    return(DBI::dbGetQuery(pool, paste(base, "WHERE username = ? ORDER BY [date] DESC, created_at DESC"),
                           params = list(username)))
  if (!is.null(username) && !is.null(month))
    return(DBI::dbGetQuery(pool, paste(base, "WHERE username = ? AND month = ? ORDER BY [date] DESC, created_at DESC"),
                           params = list(username, month)))
  if (is.null(username) && !is.null(month))
    return(DBI::dbGetQuery(pool, paste(base, "WHERE month = ?"),
                           params = list(month)))
}

# Balances (computed on the fly)
db_balances <- function(usernames = NULL, months = NULL) {
  i_tbl <- qt("incomes"); e_tbl <- qt("expenses")
  sql <- sprintf("
    WITH inc AS (
      SELECT username, month, SUM(income_amount) AS income_total
        FROM %s GROUP BY username, month
    ),
    exp AS (
      SELECT username, month, SUM(expense_amount) AS expense_total
        FROM %s GROUP BY username, month
    )
    SELECT COALESCE(inc.username, exp.username) AS username,
           COALESCE(inc.month,    exp.month)    AS month,
           COALESCE(inc.income_total, 0)        AS income_total,
           COALESCE(exp.expense_total, 0)       AS expense_total,
           COALESCE(inc.income_total, 0) - COALESCE(exp.expense_total, 0) AS balance
      FROM inc
 FULL OUTER JOIN exp
        ON inc.username = exp.username AND inc.month = exp.month
    WHERE 1 = 1
  ", i_tbl, e_tbl)
  
  params <- list()
  if (!is.null(usernames) && length(usernames) > 0) {
    sql <- paste0(sql, " AND COALESCE(inc.username, exp.username) IN (", paste(rep("?", length(usernames)), collapse=","), ")")
    params <- c(params, as.list(usernames))
  }
  if (!is.null(months) && length(months) > 0) {
    sql <- paste0(sql, " AND COALESCE(inc.month, exp.month) IN (", paste(rep("?", length(months)), collapse=","), ")")
    params <- c(params, as.list(months))
  }
  sql <- paste0(sql, " ORDER BY username, month")
  DBI::dbGetQuery(pool, sql, params = params)
}

db_all_usernames <- function() {
  u_tbl <- qt("users")
  # Distinct usernames from users table (registry)
  users <- tryCatch(
    DBI::dbGetQuery(pool, sprintf("SELECT username FROM %s", u_tbl)),
    error = function(e) data.frame(username = character())
  )
  
  # Also union usernames that may exist only in data (defensive)
  i_tbl <- qt("incomes")
  e_tbl <- qt("expenses")
  
  inc_users <- tryCatch(
    DBI::dbGetQuery(pool, sprintf("SELECT DISTINCT username FROM %s", i_tbl)),
    error = function(e) data.frame(username = character())
  )
  exp_users <- tryCatch(
    DBI::dbGetQuery(pool, sprintf("SELECT DISTINCT username FROM %s", e_tbl)),
    error = function(e) data.frame(username = character())
  )
  
  # Combine and sort
  sort(unique(c(users$username, inc_users$username, exp_users$username)))
}
