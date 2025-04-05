#' Adjust Share Count for Stock Splits
#'
#' @param financial_data Dataframe containing financial statements with dates and shares_outstanding
#' @param splits_data Dataframe containing split history
#' @return Adjusted financial data with split-adjusted share counts
#'
adjust_shares_for_splits <- function(financial_data, splits_data) {
  if (nrow(splits_data) == 0) {
    return(financial_data)
  }
  
  # For each financial report date, calculate the cumulative split factor
  # for all splits that occurred AFTER that report date
  financial_data <- financial_data %>%
    dplyr::mutate(
      cumulative_split_factor = sapply(date, function(report_date) {
        relevant_splits <- splits_data %>%
          dplyr::filter(effective_date > report_date)
        
        if (nrow(relevant_splits) == 0) {
          return(1)
        } else {
          # Multiply all split factors together
          return(prod(relevant_splits$split_factor))
        }
      }),
      # Multiply historical shares by the cumulative split factor to get modern equivalent
      shares_outstanding_adjusted = shares_outstanding * cumulative_split_factor  # Changed from division to multiplication
    )
  
  return(financial_data)
}