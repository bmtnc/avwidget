#' Calculate TTM Values for Income Statement Items
#'
#' @param financial_data DataFrame containing quarterly financial data
#' @return DataFrame with original data plus TTM calculations for income statement items
#' @importFrom dplyr mutate arrange across
#' @importFrom roll roll_sum
calculate_ttm_metrics <- function(financial_data) {
  # Define income statement items that should be TTM'd
  ttm_columns <- c(
    "revenue",
    "gross_profit",
    "operating_income",
    "ebit",
    "interest_expense",
    "income_before_tax",
    "net_income",
    "research_development",
    "sga_expense",
    "operating_expense",
    "depreciation_amortization"
  )
  
  # Identify which columns actually exist in the data
  available_columns <- intersect(names(financial_data), ttm_columns)
  
  if(length(available_columns) == 0) {
    stop("No TTM-eligible columns found in financial_data")
  }
  
  # Calculate TTM for each available metric and return
  financial_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      across(
        all_of(available_columns),
        ~as.numeric(.) # Ensure numeric
      ),
      across(
        all_of(available_columns),
        ~roll::roll_sum(., width = 4, min_obs = 4),
        .names = "{.col}_ttm"
      )
    )
}