# app.R
# Personal Finance Tracker (Income, Expenses, Monthly Balance)
# Multi-user by typed username (created on first use)
# Vaibhav Sunil Borkar – Shiny App

# ───────────────────────────────────────────────────────────────
# Packages
# ───────────────────────────────────────────────────────────────
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(DT)
library(tidyr)
library(ggplot2)

# ───────────────────────────────────────────────────────────────
# Config & helpers
# ───────────────────────────────────────────────────────────────
DATA_DIR    <- "data"
USERS_CSV   <- file.path(DATA_DIR, "users.csv")
INCOME_CSV  <- file.path(DATA_DIR, "incomes.csv")
EXPENSE_CSV <- file.path(DATA_DIR, "expenses.csv")
BALANCE_CSV <- file.path(DATA_DIR, "balances.csv")
options(scipen = 999)  # larger value → less scientific notation

ensure_data_files <- function() {
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
  
  if (!file.exists(USERS_CSV)) {
    write_csv(
      tibble(
        username   = character(),
        created_at = character()
      ),
      USERS_CSV
    )
  }
  
  if (!file.exists(INCOME_CSV)) {
    write_csv(
      tibble(
        id            = character(),
        username      = character(),
        month         = character(),     # YYYY-MM
        income_amount = double(),
        source        = character(),
        created_at    = character()
      ),
      INCOME_CSV
    )
  }
  
  if (!file.exists(EXPENSE_CSV)) {
    write_csv(
      tibble(
        id             = character(),
        username       = character(),
        date           = character(),    # YYYY-MM-DD
        month          = character(),    # YYYY-MM
        category       = character(),
        description    = character(),
        expense_amount = double(),
        created_at     = character()
      ),
      EXPENSE_CSV
    )
  }
  
  if (!file.exists(BALANCE_CSV)) {
    write_csv(
      tibble(
        username     = character(),
        month        = character(),      # YYYY-MM
        income_total = double(),
        expense_total= double(),
        balance      = double()
      ),
      BALANCE_CSV
    )
  }
}

# Safe, simple ID
new_id <- function() paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1000:9999, 1))

# Month key from Date
to_month_key <- function(date) format(as.Date(date), "%Y-%m")

# Align columns helper for robust bind_rows
align_columns <- function(df, cols) {
  for (c in cols) if (!c %in% names(df)) df[[c]] <- NA
  df[, cols]
}

# Type-safe appender: enforce character/numeric columns before binding
append_rows_and_save <- function(new_rows, path) {
  # Columns that should be character in CSVs
  char_cols <- c("id","username","date","month","source","category","description","created_at","type")
  # Columns that should be numeric
  num_cols  <- c("income_amount","expense_amount","income_total","expense_total","balance","credit","debit")
  
  # Coerce NEW rows
  for (c in intersect(names(new_rows), char_cols)) new_rows[[c]] <- as.character(new_rows[[c]])
  for (c in intersect(names(new_rows), num_cols))  new_rows[[c]] <- suppressWarnings(as.numeric(new_rows[[c]]))
  
  if (file.exists(path) && file.size(path) > 0) {
    old <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    
    # Coerce OLD rows
    for (c in intersect(names(old), char_cols)) old[[c]] <- as.character(old[[c]])
    for (c in intersect(names(old), num_cols))  old[[c]] <- suppressWarnings(as.numeric(old[[c]]))
    
    # Align & bind
    all_cols <- union(names(old), names(new_rows))
    old <- align_columns(old, all_cols)
    new_rows <- align_columns(new_rows, all_cols)
    out <- dplyr::bind_rows(old, new_rows)
  } else {
    out <- new_rows
  }
  
  tmp <- paste0(path, ".tmp")
  readr::write_csv(out, tmp)
  file.rename(tmp, path)
}

# Readers with backfill for legacy files (no username) + type coercion
read_users <- function(path = USERS_CSV) {
  if (!file.exists(path) || file.size(path) == 0) return(tibble())
  suppressMessages(readr::read_csv(path, show_col_types = FALSE)) %>%
    mutate(username = as.character(username),
           created_at = as.character(created_at))
}

read_incomes <- function(path = INCOME_CSV) {
  if (!file.exists(path) || file.size(path) == 0) return(tibble())
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  if (!"username" %in% names(df)) df$username <- "legacy"
  df %>%
    mutate(
      id            = as.character(id),
      username      = as.character(username),
      month         = as.character(month),
      source        = as.character(source),
      created_at    = as.character(created_at),
      income_amount = suppressWarnings(as.numeric(income_amount))
    )
}

read_expenses <- function(path = EXPENSE_CSV) {
  if (!file.exists(path) || file.size(path) == 0) return(tibble())
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  if (!"username" %in% names(df)) df$username <- "legacy"
  df %>%
    mutate(
      id             = as.character(id),
      username       = as.character(username),
      date           = as.character(date),
      month          = as.character(month),
      category       = as.character(category),
      description    = as.character(description),
      created_at     = as.character(created_at),
      expense_amount = suppressWarnings(as.numeric(expense_amount))
    )
}

# Recompute balances.csv from incomes & expenses grouped by username + month
recompute_balances <- function() {
  inc <- read_incomes()
  exp <- read_expenses()
  
  inc_summarised <- inc %>%
    mutate(month = as.character(month), username = as.character(username)) %>%
    group_by(username, month) %>%
    summarise(income_total = sum(income_amount, na.rm = TRUE), .groups = "drop")
  
  exp_summarised <- exp %>%
    mutate(month = as.character(month), username = as.character(username)) %>%
    group_by(username, month) %>%
    summarise(expense_total = sum(expense_amount, na.rm = TRUE), .groups = "drop")
  
  bal <- full_join(inc_summarised, exp_summarised, by = c("username","month")) %>%
    replace_na(list(income_total = 0, expense_total = 0)) %>%
    mutate(balance = income_total - expense_total) %>%
    arrange(username, month)
  
  tmp <- paste0(BALANCE_CSV, ".tmp")
  write_csv(bal, tmp)
  file.rename(tmp, BALANCE_CSV)
}

# Format currency
fmt_cur <- function(x, currency = "₹") {
  paste0(currency, format(round(x, 2), big.mark = ",", nsmall = 2, trim = TRUE))
}

# ───────────────────────────────────────────────────────────────
# UI
# ───────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Monthly Budget Tracker",
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        # Signed-in banner and switch user
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
  
  ensure_data_files()
  
  # Current signed-in username (filled by modal at startup)
  current_user <- reactiveVal(NULL)
  
  # Open sign-in modal
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
  
  # Show modal on app start
  open_signin_modal()
  
  # Username hint (availability)
  output$username_hint <- renderUI({
    u <- trimws(input$username_input %||% "")
    if (!nzchar(u)) return(NULL)
    usr <- read_users()
    exists <- nrow(usr %>% filter(tolower(username) == tolower(u))) > 0
    if (exists) {
      span(style="color:#16a085;", "✅ Existing user; your data will be loaded.")
    } else {
      span(style="color:#2980b9;", "ℹ️ New user; your profile will be created.")
    }
  })
  
  # Little helper
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Handle sign-in
  observeEvent(input$btn_signin, {
    u_raw <- input$username_input %||% ""
    u <- trimws(u_raw)
    validate(need(nzchar(u), "Please enter a username."))
    # Normalize storing as typed (keep case), but lookup case-insensitive
    users <- read_users()
    exists <- nrow(users %>% filter(tolower(username) == tolower(u))) > 0
    
    if (!exists) {
      # create user
      new_user <- tibble(
        username = u,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      append_rows_and_save(new_user, USERS_CSV)
    }
    
    current_user(u)
    removeModal()
    # Ensure balances reflect any legacy rows + this user
    recompute_balances()
  })
  
  # Allow switching user later
  observeEvent(input$switch_user, {
    open_signin_modal(preset = current_user() %||% "")
  })
  
  # Who am I banner
  output$whoami <- renderText({
    cu <- current_user()
    if (is.null(cu)) "—" else cu
  })
  
  # Live readers (auto-refresh files every 2 seconds)
  incomes_react  <- reactiveFileReader(2000, session, INCOME_CSV, function(f) read_incomes(f))
  expenses_react <- reactiveFileReader(2000, session, EXPENSE_CSV, function(f) read_expenses(f))
  balances_react <- reactiveFileReader(2000, session, BALANCE_CSV, function(file) {
    if (!file.exists(file) || file.size(file) == 0) return(tibble())
    df <- suppressMessages(readr::read_csv(file, show_col_types = FALSE))
    if (!"username" %in% names(df)) {
      recompute_balances()
      df <- suppressMessages(readr::read_csv(file, show_col_types = FALSE))
    }
    df
  })
  
  # Month picker options for CURRENT USER ONLY
  output$month_picker <- renderUI({
    req(!is.null(current_user()))
    uid <- current_user()
    inc <- tryCatch(incomes_react(),  error = function(e) tibble())
    exp <- tryCatch(expenses_react(), error = function(e) tibble())
    months <- c(inc$month[inc$username == uid], exp$month[exp$username == uid]) %>%
      unique() %>% sort()
    if (length(months) == 0) months <- to_month_key(Sys.Date())
    selectInput("selected_month", "Select month (YYYY-MM)", choices = months, selected = tail(months, 1))
  })
  
  # Add Income (saves with username)
  observeEvent(input$btn_add_income, {
    req(!is.null(current_user()))
    req(!is.na(input$income_amount), input$income_amount >= 0)
    mkey <- to_month_key(input$income_month)
    new_row <- tibble(
      id            = new_id(),
      username      = current_user(),
      month         = mkey,
      income_amount = as.numeric(input$income_amount),
      source        = ifelse(nchar(trimws(input$income_source)) == 0, NA_character_, trimws(input$income_source)),
      created_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    append_rows_and_save(new_row, INCOME_CSV)
    recompute_balances()
    showNotification(paste0("Income saved for ", mkey, " (", fmt_cur(input$income_amount), ")."), type = "message")
    updateNumericInput(session, "income_amount", value = NA_real_)
  })
  
  # Add Expense (saves with username)
  observeEvent(input$btn_add_expense, {
    req(!is.null(current_user()))
    req(!is.na(input$expense_amount), input$expense_amount >= 0, !is.null(input$expense_date))
    mkey <- to_month_key(input$expense_date)
    new_row <- tibble(
      id             = new_id(),
      username       = current_user(),
      date           = format(as.Date(input$expense_date), "%Y-%m-%d"),
      month          = mkey,
      category       = input$expense_category,
      description    = ifelse(nchar(trimws(input$expense_desc)) == 0, NA_character_, trimws(input$expense_desc)),
      expense_amount = as.numeric(input$expense_amount),
      created_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    append_rows_and_save(new_row, EXPENSE_CSV)
    recompute_balances()
    showNotification(paste0("Expense saved for ", mkey, " (", fmt_cur(input$expense_amount), ")."), type = "message")
    updateNumericInput(session, "expense_amount", value = NA_real_)
  })
  
  # Cards (selected month, current user)
  output$card_income <- renderText({
    req(!is.null(current_user()))
    sel <- req(input$selected_month)
    uid <- current_user()
    bal <- tryCatch(balances_react(), error = function(e) tibble())
    val <- bal %>% filter(username == uid, month == sel) %>% pull(income_total)
    if (length(val) == 0) val <- 0
    fmt_cur(val)
  })
  
  output$card_expense <- renderText({
    req(!is.null(current_user()))
    sel <- req(input$selected_month)
    uid <- current_user()
    bal <- tryCatch(balances_react(), error = function(e) tibble())
    val <- bal %>% filter(username == uid, month == sel) %>% pull(expense_total)
    if (length(val) == 0) val <- 0
    fmt_cur(val)
  })
  
  output$card_balance <- renderText({
    req(!is.null(current_user()))
    sel <- req(input$selected_month)
    uid <- current_user()
    bal <- tryCatch(balances_react(), error = function(e) tibble())
    val <- bal %>% filter(username == uid, month == sel) %>% pull(balance)
    if (length(val) == 0) val <- 0
    fmt_cur(val)
  })
  
  # Monthly trend (current user)
  output$plot_trend <- renderPlot({
    req(!is.null(current_user()))
    uid <- current_user()
    bal <- tryCatch(balances_react(), error = function(e) tibble()) %>% filter(username == uid)
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
    req(!is.null(current_user()))
    sel <- req(input$selected_month)
    uid <- current_user()
    inc <- tryCatch(incomes_react(), error = function(e) tibble()) %>%
      filter(username == uid, month == sel) %>%
      arrange(desc(created_at))
    datatable(inc, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$tbl_expenses <- renderDT({
    req(!is.null(current_user()))
    sel <- req(input$selected_month)
    uid <- current_user()
    exp <- tryCatch(expenses_react(), error = function(e) tibble()) %>%
      filter(username == uid, month == sel) %>%
      arrange(desc(date), desc(created_at))
    datatable(exp, rownames = FALSE, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Downloads (current user only)
  output$dl_incomes <- downloadHandler(
    filename = function() "incomes.csv",
    content = function(file) {
      uid <- current_user(); req(!is.null(uid))
      inc <- read_incomes() %>% filter(username == uid)
      write_csv(inc, file)
    }
  )
  output$dl_expenses <- downloadHandler(
    filename = function() "expenses.csv",
    content = function(file) {
      uid <- current_user(); req(!is.null(uid))
      exp <- read_expenses() %>% filter(username == uid)
      write_csv(exp, file)
    }
  )
  output$dl_balances <- downloadHandler(
    filename = function() "balances.csv",
    content = function(file) {
      recompute_balances()
      uid <- current_user(); req(!is.null(uid))
      bal <- suppressMessages(read_csv(BALANCE_CSV, show_col_types = FALSE)) %>% filter(username == uid)
      write_csv(bal, file)
    }
  )
  
  # Combined ledger for export (current user only)
  output$dl_ledger <- downloadHandler(
    filename = function() "combined_ledger.csv",
    content = function(file) {
      uid <- current_user(); req(!is.null(uid))
      inc <- read_incomes()  %>% filter(username == uid)
      exp <- read_expenses() %>% filter(username == uid)
      
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
          type,        # not present; set explicitly below to avoid bind issues
          date,
          month,
          category,
          description,
          credit,
          debit,
          created_at
        )
      
      # Build expense ledger cleanly with types aligned
      exp_ledger <- exp %>%
        transmute(
          type        = "Expense",
          date        = date,
          month       = month,
          category    = category,
          description = description,
          credit      = NA_real_,
          debit       = expense_amount,
          created_at  = created_at
        )
      
      # Enforce column types before bind
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
  
  # Initial recompute to ensure balances.csv is consistent
  observe({
    recompute_balances()
  })
}

# ───────────────────────────────────────────────────────────────
# Run
# ───────────────────────────────────────────────────────────────
shinyApp(ui, server)