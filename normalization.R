
library(readr)
library(dplyr)
library(stringr)

DATA_DIR <- "data"
INCOME_CSV <- file.path(DATA_DIR, "incomes.csv")
EXPENSE_CSV <- file.path(DATA_DIR, "expenses.csv")

# helper: normalize numbers like "₹1,200.50" -> 1200.50; "1.200,50" (EU) -> 1200.50 if needed
to_num <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  # Remove currency symbols and spaces
  x <- str_remove_all(x, "[^0-9,.-]")
  # If there are both comma and dot, assume comma is thousands sep
  has_comma <- str_detect(x, ",")
  has_dot   <- str_detect(x, "\\.")
  x <- ifelse(has_comma & has_dot, str_replace_all(x, ",", ""), x)
  # If only comma present (e.g., "1.234,56"), treat comma as decimal
  x <- ifelse(has_comma & !has_dot, str_replace_all(x, ",", "."), x)
  suppressWarnings(as.numeric(x))
}

# Clean incomes
if (file.exists(INCOME_CSV) && file.size(INCOME_CSV) > 0) {
  inc <- suppressMessages(read_csv(INCOME_CSV, show_col_types = FALSE))
  if ("income_amount" %in% names(inc)) {
    inc <- inc %>% mutate(income_amount = to_num(income_amount))
  }
  write_csv(inc, INCOME_CSV)
}

# Clean expenses
if (file.exists(EXPENSE_CSV) && file.size(EXPENSE_CSV) > 0) {
  exp <- suppressMessages(read_csv(EXPENSE_CSV, show_col_types = FALSE))
  if ("expense_amount" %in% names(exp)) {
    exp <- exp %>% mutate(expense_amount = to_num(expense_amount))
  }
  write_csv(exp, EXPENSE_CSV)
}

message("Cleanup complete. Restart your Shiny app.")
