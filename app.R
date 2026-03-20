# app.R
# Personal Finance Tracker (Income, Expenses, Monthly Balance)
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
DATA_DIR <- "data"
INCOME_CSV <- file.path(DATA_DIR, "incomes.csv")
EXPENSE_CSV <- file.path(DATA_DIR, "expenses.csv")
BALANCE_CSV <- file.path(DATA_DIR, "balances.csv")

ensure_data_files <- function() {
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
  if (!file.exists(INCOME_CSV)) {
    write_csv(
      tibble(
        id = character(),
        month = character(),     # YYYY-MM
        income_amount = double(),
        source = character(),
        created_at = character()
      ),
      INCOME_CSV
    )
  }
  if (!file.exists(EXPENSE_CSV)) {
    write_csv(
      tibble(
        id = character(),
        date = character(),       # YYYY-MM-DD
        month = character(),      # YYYY-MM
        category = character(),
        description = character(),
        expense_amount = double(),
        created_at = character()
      ),
      EXPENSE_CSV
    )
  }
  if (!file.exists(BALANCE_CSV)) {
    write_csv(
      tibble(
        month = character(),      # YYYY-MM
        income_total = double(),
        expense_total = double(),
        balance = double()
      ),
      BALANCE_CSV
    )
  }
}

# safe, simple ID
new_id <- function() {
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(1000:9999, 1))
}

# Month key from Date
to_month_key <- function(date) format(as.Date(date), "%Y-%m")

# Safe write: read old, bind new, write tmp, rename
append_rows_and_save <- function(new_rows, path) {
  # Identify which amount column exists
  amt_col <- intersect(names(new_rows), c("income_amount", "expense_amount"))
  if (length(amt_col) == 1) {
    # Coerce the new rows' amount to numeric
    new_rows[[amt_col]] <- suppressWarnings(as.numeric(new_rows[[amt_col]]))
  }
  
  if (file.exists(path) && file.size(path) > 0) {
    old <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    
    # If the old file has the same amount column, coerce it too
    if (length(amt_col) == 1 && amt_col %in% names(old)) {
      old[[amt_col]] <- suppressWarnings(as.numeric(old[[amt_col]]))
    }
    
    out <- dplyr::bind_rows(old, new_rows)
  } else {
    out <- new_rows
  }
  
  tmp <- paste0(path, ".tmp")
  readr::write_csv(out, tmp)
  file.rename(tmp, path)
}

# Recompute balances.csv from incomes & expenses
recompute_balances <- function() {
  inc <- if (file.exists(INCOME_CSV)) suppressMessages(read_csv(INCOME_CSV, show_col_types = FALSE)) else tibble()
  exp <- if (file.exists(EXPENSE_CSV)) suppressMessages(read_csv(EXPENSE_CSV, show_col_types = FALSE)) else tibble()
  
  inc_summarised <- inc %>%
    mutate(month = as.character(month)) %>%
    group_by(month) %>%
    summarise(income_total = sum(as.numeric(income_amount), na.rm = TRUE), .groups = "drop")
  
  exp_summarised <- exp %>%
    mutate(month = as.character(month)) %>%
    group_by(month) %>%
    summarise(expense_total = sum(as.numeric(expense_amount), na.rm = TRUE), .groups = "drop")
  
  bal <- full_join(inc_summarised, exp_summarised, by = "month") %>%
    replace_na(list(income_total = 0, expense_total = 0)) %>%
    mutate(balance = income_total - expense_total) %>%
    arrange(month)
  
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
          column(4, wellPanel(h4("Income (Selected Month)"), h2(textOutput("card_income")))),
          column(4, wellPanel(h4("Expenses (Selected Month)"), h2(textOutput("card_expense")))),
          column(4, wellPanel(h4("Balance (Selected Month)"), h2(textOutput("card_balance"))))
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
            choices = c("Groceries", "Rent", "Utilities", "Transport", "Dining", "Health", "Education", "Shopping", "Entertainment", "Other"),
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
  
  # Live readers (auto-refresh files every 2 seconds)
  incomes_react <- reactiveFileReader(2000, session, INCOME_CSV, read_csv, show_col_types = FALSE)
  expenses_react <- reactiveFileReader(2000, session, EXPENSE_CSV, read_csv, show_col_types = FALSE)
  balances_react <- reactiveFileReader(2000, session, BALANCE_CSV, read_csv, show_col_types = FALSE)
  
  # Month picker options from balances + incomes + expenses
  output$month_picker <- renderUI({
    inc <- tryCatch(incomes_react(), error = function(e) tibble())
    exp <- tryCatch(expenses_react(), error = function(e) tibble())
    months <- c(inc$month, exp$month) %>% unique() %>% sort()
    if (length(months) == 0) {
      # default to current month
      months <- to_month_key(Sys.Date())
    }
    selectInput("selected_month", "Select month (YYYY-MM)", choices = months, selected = tail(months, 1))
  })
  
  # Add Income
  observeEvent(input$btn_add_income, {
    req(!is.na(input$income_amount), input$income_amount >= 0)
    mkey <- to_month_key(input$income_month)
    new_row <- tibble(
      id = new_id(),
      month = mkey,
      income_amount = as.numeric(input$income_amount),
      source = ifelse(nchar(trimws(input$income_source)) == 0, NA_character_, trimws(input$income_source)),
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    append_rows_and_save(new_row, INCOME_CSV)
    recompute_balances()
    showNotification(paste0("Income saved for ", mkey, " (", fmt_cur(input$income_amount), ")."), type = "message")
    # Reset amount field
    updateNumericInput(session, "income_amount", value = NA_real_)
  })
  
  # Add Expense
  observeEvent(input$btn_add_expense, {
    req(!is.na(input$expense_amount), input$expense_amount >= 0, !is.null(input$expense_date))
    mkey <- to_month_key(input$expense_date)
    new_row <- tibble(
      id = new_id(),
      date = format(as.Date(input$expense_date), "%Y-%m-%d"),
      month = mkey,
      category = input$expense_category,
      description = ifelse(nchar(trimws(input$expense_desc)) == 0, NA_character_, trimws(input$expense_desc)),
      expense_amount = as.numeric(input$expense_amount),
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    append_rows_and_save(new_row, EXPENSE_CSV)
    recompute_balances()
    showNotification(paste0("Expense saved for ", mkey, " (", fmt_cur(input$expense_amount), ")."), type = "message")
    # Reset amount field
    updateNumericInput(session, "expense_amount", value = NA_real_)
  })
  
  # Cards (selected month)
  output$card_income <- renderText({
    sel <- req(input$selected_month)
    bal <- tryCatch(balances_react(), error = function(e) tibble())
    val <- bal %>% filter(month == sel) %>% pull(income_total)
    if (length(val) == 0) val <- 0
    fmt_cur(val)
  })
  
  output$card_expense <- renderText({
    sel <- req(input$selected_month)
    bal <- tryCatch(balances_react(), error = function(e) tibble())
    val <- bal %>% filter(month == sel) %>% pull(expense_total)
    if (length(val) == 0) val <- 0
    fmt_cur(val)
  })
  
  output$card_balance <- renderText({
    sel <- req(input$selected_month)
    bal <- tryCatch(balances_react(), error = function(e) tibble())
    val <- bal %>% filter(month == sel) %>% pull(balance)
    if (length(val) == 0) val <- 0
    fmt_cur(val)
  })
  
  # Monthly trend
  output$plot_trend <- renderPlot({
    bal <- tryCatch(balances_react(), error = function(e) tibble())
    req(nrow(bal) > 0)
    bal2 <- bal %>%
      mutate(
        month_date = ymd(paste0(month, "-01"))
      ) %>%
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
  
  # Tables (selected month)
  output$tbl_incomes <- renderDT({
    sel <- req(input$selected_month)
    inc <- tryCatch(incomes_react(), error = function(e) tibble())
    inc2 <- inc %>%
      filter(month == sel) %>%
      arrange(desc(created_at))
    datatable(
      inc2,
      rownames = FALSE,
      options = list(pageLength = 5, autoWidth = TRUE)
    )
  })
  
  output$tbl_expenses <- renderDT({
    sel <- req(input$selected_month)
    exp <- tryCatch(expenses_react(), error = function(e) tibble())
    exp2 <- exp %>%
      filter(month == sel) %>%
      arrange(desc(date), desc(created_at))
    datatable(
      exp2,
      rownames = FALSE,
      options = list(pageLength = 5, autoWidth = TRUE)
    )
  })
  
  # Downloads
  output$dl_incomes <- downloadHandler(
    filename = function() "incomes.csv",
    content = function(file) file.copy(INCOME_CSV, file, overwrite = TRUE)
  )
  output$dl_expenses <- downloadHandler(
    filename = function() "expenses.csv",
    content = function(file) file.copy(EXPENSE_CSV, file, overwrite = TRUE)
  )
  output$dl_balances <- downloadHandler(
    filename = function() "balances.csv",
    content = function(file) {
      recompute_balances()
      file.copy(BALANCE_CSV, file, overwrite = TRUE)
    }
  )
  
  # Combined ledger for export
  output$dl_ledger <- downloadHandler(
    filename = function() "combined_ledger.csv",
    content = function(file) {
      inc <- if (file.exists(INCOME_CSV)) suppressMessages(read_csv(INCOME_CSV, show_col_types = FALSE)) else tibble()
      exp <- if (file.exists(EXPENSE_CSV)) suppressMessages(read_csv(EXPENSE_CSV, show_col_types = FALSE)) else tibble()
      
      inc_ledger <- inc %>%
        transmute(
          type = "Income",
          date = paste0(month, "-01"),
          month,
          category = source,
          description = NA_character_,
          credit = income_amount,
          debit = NA_real_,
          created_at
        )
      
      exp_ledger <- exp %>%
        transmute(
          type = "Expense",
          date,
          month,
          category,
          description,
          credit = NA_real_,
          debit = expense_amount,
          created_at
        )
      
      ledger <- bind_rows(inc_ledger, exp_ledger) %>%
        arrange(date, desc(type)) # show income before expenses on same day
      
      write_csv(ledger, file)
    }
  )
  
  # Initial recompute to ensure balances.csv is consistent
  observe({
    # run once at start
    recompute_balances()
  })
}

# ───────────────────────────────────────────────────────────────
# Run
# ───────────────────────────────────────────────────────────────
shinyApp(ui, server)

