#' Calculate Historical Per-Share Financial Metrics 
#'
#' Takes financial time series data and calculates key per-share metrics.
#' All monetary values are calculated on a per-share basis.
#'
#' @param financial_data A tibble containing financial statements data
#' @return A tibble with calculated per-share metrics
#' 
#' @importFrom dplyr mutate select
#' @importFrom scales dollar
calculate_per_share_metrics <- function(financial_data) {
  # Input validation
  required_cols <- c("date", "revenue", "gross_profit", "ebit", 
                    "long_term_debt", "cash", "shares_outstanding")
  
  missing_cols <- setdiff(required_cols, names(financial_data))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required columns: ", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Calculate per-share metrics
  per_share_data <- financial_data %>%
    dplyr::mutate(
      # Core per-share metrics
      revenue_per_share = revenue / shares_outstanding,
      gross_profit_per_share = gross_profit / shares_outstanding,
      ebit_per_share = ebit / shares_outstanding,
      debt_per_share = long_term_debt / shares_outstanding,
      cash_per_share = cash / shares_outstanding,
      
      # Format for display
      revenue_per_share = scales::dollar(revenue_per_share),
      gross_profit_per_share = scales::dollar(gross_profit_per_share),
      ebit_per_share = scales::dollar(ebit_per_share),
      debt_per_share = scales::dollar(debt_per_share),
      cash_per_share = scales::dollar(cash_per_share)
    ) %>%
    dplyr::select(
      date,
      revenue_per_share,
      gross_profit_per_share, 
      ebit_per_share,
      debt_per_share,
      cash_per_share
    ) %>%
    dplyr::arrange(desc(date))
  
  return(per_share_data)
}