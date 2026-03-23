# app.R
# Personal Finance Tracker (Income, Expenses, Monthly Balance)
# Multi-user by typed username (created on first use)
# Azure SQL (ODBC) backend
# Vaibhav Sunil Borkar – Shiny App

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
if (Sys.getenv("R_CONFIG_ACTIVE") == "connect_cloud") {
  pool <- pool::dbPool(
    drv = odbc::odbc(),
    .connection_string = paste0(
      "Driver=FreeTDS;",
      "TDS_Version=7.2;",
      "Server=", Sys.getenv("SERVER"), ";",
      "Port=1433;",
      "Database=", Sys.getenv("SERVER"), ";",
      "Uid=", Sys.getenv("AZURE_SQL_UID"), ";",
      "Pwd=", Sys.getenv("AZURE_SQL_PWD"), ";",
      "Encrypt=yes;",
      "TrustServerCertificate=no;",
      "Connection Timeout=30;"
    )
  )
} else {
  pool <- pool::dbPool(
    drv      = odbc::odbc(),
    Driver   = "ODBC Driver 18 for SQL Server",
    Server   = Sys.getenv("SERVER"),
    Database = Sys.getenv("DATABASE"),
    UID      = Sys.getenv("AZURE_SQL_UID"),
    PWD      = Sys.getenv("AZURE_SQL_PWD"),  # set in .Renviron or hosting secret
    Encrypt  = "yes",
    TrustServerCertificate = "no",
    Timeout  = 30
  )
  
}



onStop(function() {
  pool::poolClose(pool)
})

# Helper: NULL-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ───────────────────────────────────────────────────────────────
# Helpers
# ───────────────────────────────────────────────────────────────

DB_SCHEMA <- Sys.getenv("SCHEMA_NAME")

# Quote [schema].[table] safely for SQL Server via DBI
qt <- function(tbl) {
  DBI::dbQuoteIdentifier(pool, DBI::Id(schema = DB_SCHEMA, table = tbl)) |> as.character()
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

# ───────────────────────────────────────────────────────────────
# UI
# ───────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Monthly Budget Tracker",
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Combined Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Select Scope"),
        uiOutput("combo_user_selector"),
        uiOutput("combo_month_selector"),
        hr(),
        downloadButton("dl_combo_balances", "Download combined balances.csv"),
        br(), br(),
        downloadButton("dl_combo_ledger", "Download combined_ledger.csv")
      ),
      mainPanel(
        fluidRow(
          column(4, wellPanel(h4("Total Income"),  h2(textOutput("combo_card_income")))),
          column(4, wellPanel(h4("Total Expenses"), h2(textOutput("combo_card_expense")))),
          column(4, wellPanel(h4("Total Balance"),  h2(textOutput("combo_card_balance"))))
        ),
        hr(),
        h4("Trend (Selected Users)"),
        plotOutput("combo_plot_trend", height = 300),
        hr(),
        h4("Breakdown by User (Selected Months)"),
        DTOutput("combo_tbl_by_user"),
        hr(),
        h4("Detailed Incomes"),
        DTOutput("combo_tbl_incomes"),
        hr(),
        h4("Detailed Expenses"),
        DTOutput("combo_tbl_expenses")
      )
    )
  ),
  
  tabPanel(
    "Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          strong("Signed in as:"),
          textOutput("whoami", inline = TRUE),
          actionLink("switch_user", " (switch)", style = "margin-left:6px;")
        ),
        hr(),
        uiOutput("month_picker"),
        hr(),
        downloadButton("dl_incomes", "Download incomes.csv"),
        br(), br(),
        downloadButton("dl_expenses", "Download expenses.csv"),
        br(), br(),
        downloadButton("dl_balances", "Download balances.csv"),
        br(), br(),
        downloadButton("dl_ledger", "Download combined_ledger.csv")
      ),
      mainPanel(
        fluidRow(
          column(4, wellPanel(h4("Income (Selected Month)"),  h2(textOutput("card_income")))),
          column(4, wellPanel(h4("Expenses (Selected Month)"), h2(textOutput("card_expense")))),
          column(4, wellPanel(h4("Balance (Selected Month)"),  h2(textOutput("card_balance"))))
        ),
        hr(),
        h4("Monthly Trend"),
        plotOutput("plot_trend", height = 300),
        hr(),
        h4("Incomes (Selected Month)"),
        DTOutput("tbl_incomes"),
        hr(),
        h4("Expenses (Selected Month)"),
        DTOutput("tbl_expenses")
      )
    )
  ),
  
  tabPanel(
    "Add Income",
    fluidRow(
      column(
        6,
        wellPanel(
          h3("New Income"),
          dateInput("income_month", "Income month", value = Sys.Date(), format = "yyyy-mm-dd"),
          numericInput("income_amount", "Amount", value = NA_real_, min = 0, step = 0.01),
          textInput("income_source", "Source (optional)", placeholder = "Salary, Bonus, etc."),
          actionButton("btn_add_income", "Save Income", class = "btn btn-success")
        )
      ),
      column(
        6,
        wellPanel(
          h4("Tip"),
          p("Income is saved per month (YYYY-MM). Adding multiple income entries in the same month is OK; totals are summed."),
          p("Balances update immediately after saving.")
        )
      )
    )
  ),
  
  tabPanel(
    "Add Expense",
    fluidRow(
      column(
        6,
        wellPanel(
          h3("New Expense"),
          dateInput("expense_date", "Expense date", value = Sys.Date(), format = "yyyy-mm-dd"),
          selectInput(
            "expense_category", "Category",
            choices = c("Groceries", "Rent", "Utilities", "Transport", "Dining", "Health", "Education", "Shopping", "Entertainment", "EMI", "Credit Card Bill", "Other"),
            selected = "Groceries"
          ),
          textInput("expense_desc", "Description (optional)", placeholder = "Note or merchant"),
          numericInput("expense_amount", "Amount", value = NA_real_, min = 0, step = 0.01),
          actionButton("btn_add_expense", "Save Expense", class = "btn btn-primary")
        )
      ),
      column(
        6,
        wellPanel(
          h4("Notes"),
          tags$ul(
            tags$li("Expenses are tied to their actual date, and also grouped into a month key."),
            tags$li("If you add expenses for a month with no income yet, the balance will be negative (which is allowed).")
          )
        )
      )
    )
  )
)

# ───────────────────────────────────────────────────────────────
# Server
# ───────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Current signed-in username (filled by modal at startup)
  current_user <- reactiveVal(NULL)
  
  # Sign-in modal
  open_signin_modal <- function(preset = "") {
    showModal(modalDialog(
      title = "Sign in",
      tagList(
        p("Enter your username. If it doesn't exist, we'll create it."),
        textInput("username_input", "Username", value = preset, placeholder = "e.g., vaibhav"),
        uiOutput("username_hint")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_signin", "Continue", class = "btn btn-primary")
      ),
      easyClose = FALSE
    ))
  }
  open_signin_modal()
  
  # Username hint (availability)
  output$username_hint <- renderUI({
    u <- trimws(input$username_input %||% "")
    if (!nzchar(u)) return(NULL)
    exists <- FALSE
    if (nzchar(u)) exists <- db_user_exists(u)
    if (exists) {
      span(style="color:#16a085;", "✅ Existing user; your data will be loaded.")
    } else {
      span(style="color:#2980b9;", "ℹ️ New user; your profile will be created.")
    }
  })
  
  # Handle sign-in
  observeEvent(input$btn_signin, {
    u <- trimws(input$username_input %||% "")
    validate(need(nzchar(u), "Please enter a username."))
    if (!db_user_exists(u)) db_create_user(u)
    current_user(u)
    removeModal()
  })
  
  # Switch user later
  observeEvent(input$switch_user, {
    open_signin_modal(preset = current_user() %||% "")
  })
  
  # Banner
  output$whoami <- renderText({
    cu <- current_user()
    if (is.null(cu)) "—" else cu
  })
  
  # ------------- Combined Dashboard selectors ------------------
  
  output$combo_user_selector <- renderUI({
    users <- db_all_usernames()
    selectizeInput(
      "combo_users",
      "Users",
      choices = sort(users),
      multiple = TRUE,
      options = list(placeholder = "Choose one or more users")
    )
  })
  
  output$combo_month_selector <- renderUI({
    req(length(input$combo_users) > 0)
    
    # Schema-qualified tables
    i_tbl <- qt("incomes")
    e_tbl <- qt("expenses")
    
    # IN list placeholders matching number of selected users
    placeholders <- paste(rep("?", length(input$combo_users)), collapse = ",")
    
    # Build the SQL string with schema-qualified tables and placeholders
    sql <- sprintf("
    WITH m AS (
      SELECT username, month FROM %s
      UNION ALL
      SELECT username, month FROM %s
    )
    SELECT DISTINCT month
    FROM m
    WHERE username IN (%s)
    ORDER BY month
  ", i_tbl, e_tbl, placeholders)
    
    months <- DBI::dbGetQuery(
      pool,
      sql,
      params = as.list(input$combo_users)
    )$month
    
    if (length(months) == 0) months <- to_month_key(Sys.Date())
    
    selectizeInput(
      "combo_months",
      "Months (YYYY-MM)",
      choices   = months,
      selected  = tail(months, min(6, length(months))),
      multiple  = TRUE
    )
  })
  

  
  # Helper to paste with no spaces
  "%+%" <- function(a,b) paste0(a,b)
  
  # ---------- Combined Dashboard cards/plot/tables --------------
  combo_balances_selected <- reactive({
    req(length(input$combo_users) > 0, length(input$combo_months) > 0)
    db_balances(usernames = input$combo_users, months = input$combo_months)
  })
  
  output$combo_card_income <- renderText({
    bal <- combo_balances_selected()
    fmt_cur(sum(bal$income_total %||% 0, na.rm = TRUE))
  })
  output$combo_card_expense <- renderText({
    bal <- combo_balances_selected()
    fmt_cur(sum(bal$expense_total %||% 0, na.rm = TRUE))
  })
  output$combo_card_balance <- renderText({
    bal <- combo_balances_selected()
    fmt_cur(sum(bal$income_total - bal$expense_total, na.rm = TRUE))
  })
  
  output$combo_plot_trend <- renderPlot({
    bal <- combo_balances_selected()
    req(nrow(bal) > 0)
    bal2 <- bal %>%
      group_by(month) %>%
      summarise(
        income_total  = sum(income_total,  na.rm = TRUE),
        expense_total = sum(expense_total, na.rm = TRUE),
        balance       = income_total - expense_total,
        .groups = "drop"
      ) %>%
      mutate(month_date = ymd(paste0(month, "-01"))) %>%
      arrange(month_date)
    
    ggplot(bal2, aes(x = month_date)) +
      geom_col(aes(y = income_total, fill = "Income"), width = 25, alpha = 0.7) +
      geom_col(aes(y = -expense_total, fill = "Expenses"), width = 25, alpha = 0.7) +
      geom_line(aes(y = balance, color = "Balance"), linewidth = 1.1) +
      geom_point(aes(y = balance, color = "Balance"), size = 2) +
      scale_fill_manual(values = c("Income" = "#2ecc71", "Expenses" = "#e74c3c")) +
      scale_color_manual(values = c("Balance" = "#3498db")) +
      scale_y_continuous(labels = scales::label_comma(accuracy = 1, prefix = "₹")) +
      labs(x = "Month", y = "Amount", fill = "", color = "", title = "Combined Income / Expenses / Balance") +
      theme_minimal()
  })
  
  output$combo_tbl_by_user <- renderDT({
    bal <- combo_balances_selected()
    req(nrow(bal) > 0)
    by_user <- bal %>%
      group_by(username) %>%
      summarise(
        income_total  = sum(income_total,  na.rm = TRUE),
        expense_total = sum(expense_total, na.rm = TRUE),
        balance       = income_total - expense_total,
        .groups = "drop"
      ) %>%
      arrange(desc(balance))
    
    dt <- datatable(by_user, rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE))
    dt <- DT::formatCurrency(dt, "income_total",  currency = "₹", interval = 3, mark = ",", digits = 2)
    dt <- DT::formatCurrency(dt, "expense_total", currency = "₹", interval = 3, mark = ",", digits = 2)
    dt <- DT::formatCurrency(dt, "balance",      currency = "₹", interval = 3, mark = ",", digits = 2)
    dt
  })
  
  output$combo_tbl_incomes <- renderDT({
    req(length(input$combo_users) > 0, length(input$combo_months) > 0)
    inc <- db_read_incomes() %>%
      filter(username %in% input$combo_users, month %in% input$combo_months) %>%
      arrange(username, desc(created_at))
    dt <- datatable(inc, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
    if ("income_amount" %in% names(inc)) {
      dt <- DT::formatCurrency(dt, "income_amount", currency = "₹", interval = 3, mark = ",", digits = 2)
    }
    dt
  })
  
  output$combo_tbl_expenses <- renderDT({
    req(length(input$combo_users) > 0, length(input$combo_months) > 0)
    exp <- db_read_expenses() %>%
      filter(username %in% input$combo_users, month %in% input$combo_months) %>%
      arrange(username, desc(date), desc(created_at))
    dt <- datatable(exp, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
    if ("expense_amount" %in% names(exp)) {
      dt <- DT::formatCurrency(dt, "expense_amount", currency = "₹", interval = 3, mark = ",", digits = 2)
    }
    dt
  })
  
  # Downloads for combined view
  output$dl_combo_balances <- downloadHandler(
    filename = function() "combined_balances_filtered.csv",
    content = function(file) {
      req(length(input$combo_users) > 0, length(input$combo_months) > 0)
      out <- db_balances(usernames = input$combo_users, months = input$combo_months)
      write_csv(out, file)
    }
  )
  output$dl_combo_ledger <- downloadHandler(
    filename = function() "combined_ledger_filtered.csv",
    content = function(file) {
      req(length(input$combo_users) > 0, length(input$combo_months) > 0)
      inc <- db_read_incomes()  %>% filter(username %in% input$combo_users, month %in% input$combo_months)
      exp <- db_read_expenses() %>% filter(username %in% input$combo_users, month %in% input$combo_months)
      
      inc_ledger <- inc %>% transmute(
        type        = "Income",
        username,
        date        = paste0(month, "-01"),
        month,
        category    = source,
        description = NA_character_,
        credit      = income_amount,
        debit       = NA_real_,
        created_at
      )
      exp_ledger <- exp %>% transmute(
        type        = "Expense",
        username,
        date,
        month,
        category,
        description,
        credit      = NA_real_,
        debit       = expense_amount,
        created_at
      )
      
      # ensure consistent types
      char_cols <- c("type","username","date","month","category","description","created_at")
      num_cols  <- c("credit","debit")
      for (c in intersect(names(inc_ledger), char_cols)) inc_ledger[[c]] <- as.character(inc_ledger[[c]])
      for (c in intersect(names(exp_ledger), char_cols)) exp_ledger[[c]] <- as.character(exp_ledger[[c]])
      for (c in intersect(names(inc_ledger), num_cols))  inc_ledger[[c]] <- suppressWarnings(as.numeric(inc_ledger[[c]]))
      for (c in intersect(names(exp_ledger), num_cols))  exp_ledger[[c]] <- suppressWarnings(as.numeric(exp_ledger[[c]]))
      
      out <- bind_rows(inc_ledger, exp_ledger) %>% arrange(username, date, desc(type))
      write_csv(out, file)
    }
  )
  
  # ------------- Personal Dashboard (current user) --------------
  # Month picker options for current user
  output$month_picker <- renderUI({
    req(!is.null(current_user()))
    uid <- current_user()
    months <- db_user_months(uid)
    if (length(months) == 0) months <- to_month_key(Sys.Date())
    selectInput("selected_month", "Select month (YYYY-MM)", choices = months, selected = tail(months, 1))
  })
  
  # Add Income
  observeEvent(input$btn_add_income, {
    req(!is.null(current_user()))
    req(!is.na(input$income_amount), input$income_amount >= 0)
    mkey <- to_month_key(input$income_month)
    db_insert_income(
      id       = new_id(),
      username = current_user(),
      month    = mkey,
      amount   = as.numeric(input$income_amount),
      source   = ifelse(nchar(trimws(input$income_source)) == 0, NA_character_, trimws(input$income_source))
    )
    showNotification(paste0("Income saved for ", mkey, " (", fmt_cur(input$income_amount), ")."), type = "message")
    updateNumericInput(session, "income_amount", value = NA_real_)
  })
  
  # Add Expense
  observeEvent(input$btn_add_expense, {
    req(!is.null(current_user()))
    req(!is.na(input$expense_amount), input$expense_amount >= 0, !is.null(input$expense_date))
    mkey <- to_month_key(input$expense_date)
    db_insert_expense(
      id          = new_id(),
      username    = current_user(),
      date        = as.Date(input$expense_date),
      month       = mkey,
      category    = input$expense_category,
      description = ifelse(nchar(trimws(input$expense_desc)) == 0, NA_character_, trimws(input$expense_desc)),
      amount      = as.numeric(input$expense_amount)
    )
    showNotification(paste0("Expense saved for ", mkey, " (", fmt_cur(input$expense_amount), ")."), type = "message")
    updateNumericInput(session, "expense_amount", value = NA_real_)
  })
  
  # Cards (selected month)
  output$card_income <- renderText({
    req(!is.null(current_user()), input$selected_month)
    i_tbl <- qt("incomes")
    val <- DBI::dbGetQuery(
      pool,
      sprintf("SELECT COALESCE(SUM(income_amount), 0) AS total FROM %s WHERE username = ? AND month = ?", i_tbl),
      params = list(current_user(), input$selected_month)
    )$total
    fmt_cur(val %||% 0)
  })
  
  output$card_expense <- renderText({
    req(!is.null(current_user()), input$selected_month)
    e_tbl <- qt("expenses")
    val <- DBI::dbGetQuery(
      pool,
      sprintf("SELECT COALESCE(SUM(expense_amount), 0) AS total FROM %s WHERE username = ? AND month = ?", e_tbl),
      params = list(current_user(), input$selected_month)
    )$total
    fmt_cur(val %||% 0)
  })
  
  output$card_balance <- renderText({
    req(!is.null(current_user()), input$selected_month)
    i_tbl <- qt("incomes"); e_tbl <- qt("expenses")
    res <- DBI::dbGetQuery(
      pool,
      sprintf("
      SELECT
        (SELECT COALESCE(SUM(income_amount), 0) FROM %s WHERE username = ? AND month = ?) -
        (SELECT COALESCE(SUM(expense_amount),0) FROM %s WHERE username = ? AND month = ?) AS bal
    ", i_tbl, e_tbl),
      params = list(current_user(), input$selected_month, current_user(), input$selected_month)
    )$bal
    fmt_cur(res %||% 0)
  })
  
  # Trend (current user) – monthly series
  output$plot_trend <- renderPlot({
    req(!is.null(current_user()))
    bal <- db_balances(usernames = current_user())
    req(nrow(bal) > 0)
    bal2 <- bal %>%
      mutate(month_date = ymd(paste0(month, "-01"))) %>%
      arrange(month_date)
    
    ggplot(bal2, aes(x = month_date)) +
      geom_col(aes(y = income_total, fill = "Income"), width = 25, alpha = 0.7) +
      geom_col(aes(y = -expense_total, fill = "Expenses"), width = 25, alpha = 0.7) +
      geom_line(aes(y = balance, color = "Balance"), linewidth = 1.1) +
      geom_point(aes(y = balance, color = "Balance"), size = 2) +
      scale_fill_manual(values = c("Income" = "#2ecc71", "Expenses" = "#e74c3c")) +
      scale_color_manual(values = c("Balance" = "#3498db")) +
      labs(x = "Month", y = "Amount", fill = "", color = "", title = "Income vs Expenses and Balance by Month") +
      theme_minimal()
  })
  
  # Tables (selected month, current user)
  output$tbl_incomes <- renderDT({
    req(!is.null(current_user()), input$selected_month)
    inc <- db_read_incomes(username = current_user(), month = input$selected_month)
    dt <- datatable(inc, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
    if ("income_amount" %in% names(inc)) {
      dt <- DT::formatCurrency(dt, "income_amount", currency = "₹", interval = 3, mark = ",", digits = 2)
    }
    dt
  })
  output$tbl_expenses <- renderDT({
    req(!is.null(current_user()), input$selected_month)
    exp <- db_read_expenses(username = current_user(), month = input$selected_month)
    dt <- datatable(exp, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
    if ("expense_amount" %in% names(exp)) {
      dt <- DT::formatCurrency(dt, "expense_amount", currency = "₹", interval = 3, mark = ",", digits = 2)
    }
    dt
  })
  
  # Downloads (current user)
  output$dl_incomes <- downloadHandler(
    filename = function() "incomes.csv",
    content = function(file) {
      uid <- current_user(); req(!is.null(uid))
      inc <- db_read_incomes(username = uid)
      write_csv(inc, file)
    }
  )
  output$dl_expenses <- downloadHandler(
    filename = function() "expenses.csv",
    content = function(file) {
      uid <- current_user(); req(!is.null(uid))
      exp <- db_read_expenses(username = uid)
      write_csv(exp, file)
    }
  )
  output$dl_balances <- downloadHandler(
    filename = function() "balances.csv",
    content = function(file) {
      uid <- current_user(); req(!is.null(uid))
      bal <- db_balances(usernames = uid)
      write_csv(bal, file)
    }
  )
  output$dl_ledger <- downloadHandler(
    filename = function() "combined_ledger.csv",
    content = function(file) {
      uid <- current_user(); req(!is.null(uid))
      inc <- db_read_incomes(username = uid)
      exp <- db_read_expenses(username = uid)
      
      inc_ledger <- inc %>%
        transmute(
          type        = "Income",
          date        = paste0(month, "-01"),
          month,
          category    = source,
          description = NA_character_,
          credit      = income_amount,
          debit       = NA_real_,
          created_at
        )
      exp_ledger <- exp %>%
        transmute(
          type        = "Expense",
          date,
          month,
          category,
          description,
          credit      = NA_real_,
          debit       = expense_amount,
          created_at
        )
      # align & bind
      char_cols <- c("type","date","month","category","description","created_at")
      num_cols  <- c("credit","debit")
      for (c in intersect(names(inc_ledger), char_cols)) inc_ledger[[c]] <- as.character(inc_ledger[[c]])
      for (c in intersect(names(exp_ledger), char_cols)) exp_ledger[[c]] <- as.character(exp_ledger[[c]])
      for (c in intersect(names(inc_ledger), num_cols))  inc_ledger[[c]] <- suppressWarnings(as.numeric(inc_ledger[[c]]))
      for (c in intersect(names(exp_ledger), num_cols))  exp_ledger[[c]] <- suppressWarnings(as.numeric(exp_ledger[[c]]))
      
      ledger <- bind_rows(inc_ledger, exp_ledger) %>% arrange(date, desc(type))
      write_csv(ledger, file)
    }
  )
}

# ───────────────────────────────────────────────────────────────
# Run
# ───────────────────────────────────────────────────────────────
shinyApp(ui, server)