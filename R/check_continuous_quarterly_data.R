#' Check for Continuous Quarterly Financial Data
#'
#' Verifies that financial data contains continuous quarterly observations
#' without gaps between the oldest and most recent dates
#'
#' @param financial_data A dataframe containing quarterly financial metrics with columns:
#'   - date: Date of the financial report
#'   - gross_profit: Gross profit for the period
#'   - ebit: Earnings before interest and taxes
#'   - revenue: Total revenue
#'   And potentially other financial metrics
#' @return TRUE if data is continuous, stops with error message if gaps found
check_continuous_quarterly_data <- function(financial_data) {
  if (!inherits(financial_data, "data.frame")) {
    stop("Input must be a data.frame or tibble")
  }
  
  if (!"date" %in% names(financial_data)) {
    stop("Financial data must contain a 'date' column")
  }
  
  if (!inherits(financial_data$date, "Date")) {
    stop("The date column must be of class Date")
  }
  
  # Get actual dates from the data
  actual_dates <- sort(financial_data$date)
  
  # Get expected quarter end dates
  expected_dates <- get_quarter_end_dates(min(actual_dates), max(actual_dates))
  
  # Find missing quarters
  missing_quarters <- setdiff(expected_dates, actual_dates)
  
  if (length(missing_quarters) > 0) {
    missing_quarters_str <- paste(
      format(as.Date(missing_quarters), "%Y-%m-%d"), 
      collapse = ", "
    )
    stop(sprintf("Missing quarterly data for the following dates: %s", 
                 missing_quarters_str))
  }
  
  return(TRUE)
}