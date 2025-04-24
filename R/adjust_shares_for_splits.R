# R/adjust_shares_for_splits.R

#' Adjust Historical Shares Outstanding for Stock Splits
#'
#' Adjusts historical share counts to be comparable to the most recent share count
#' by accounting for stock splits that occurred after the historical date.
#' This is useful for calculating consistent per-share metrics over time or
#' analyzing share count trends excluding split effects.
#'
#' @param financial_data A dataframe containing at least 'date' and 'shares_outstanding' columns.
#' @param splits_data A dataframe containing 'effective_date' and 'split_factor' for stock splits.
#'        A split factor > 1 indicates a forward split (e.g., 2 for a 2:1 split).
#'        A split factor < 1 indicates a reverse split (e.g., 0.5 for a 1:2 reverse split).
#' @return A dataframe identical to financial_data but with an added column
#'         'shares_equiv_current_shares', representing the historical shares
#'         adjusted to be comparable to the current share count basis.
#' @export
#' @examples
#' \dontrun{
#' financial_data <- data.frame(
#'   date = as.Date(c("2020-12-31", "2021-12-31", "2022-12-31")),
#'   shares_outstanding = c(1000, 1000, 2000) # Shares doubled in 2022
#' )
#' splits_data <- data.frame(
#'   effective_date = as.Date("2022-06-01"),
#'   split_factor = 2 # 2:1 split
#' )
#' adjusted_data <- adjust_shares_for_splits(financial_data, splits_data)
#' print(adjusted_data)
#' #         date shares_outstanding shares_equiv_current_shares
#' # 1 2020-12-31               1000                        2000  # 1000 * 2
#' # 2 2021-12-31               1000                        2000  # 1000 * 2
#' # 3 2022-12-31               2000                        2000  # After split
#' }
adjust_shares_for_splits <- function(financial_data, splits_data) {
  # Handle empty splits case
  if (nrow(splits_data) == 0) {
    financial_data$shares_equiv_current_shares <- financial_data$shares_outstanding
    return(financial_data)
  }

  # Validate split factors - explicitly check before proceeding
  if (!is.numeric(splits_data$split_factor) || any(splits_data$split_factor <= 0)) {
    stop("Split factors must be positive numbers")
  }

  # Ensure date columns are Date type
  if (!inherits(financial_data$date, "Date")) {
      financial_data$date <- tryCatch(as.Date(financial_data$date), error = function(e) stop("financial_data$date cannot be coerced to Date"))
  }
  if (!inherits(splits_data$effective_date, "Date")) {
      splits_data$effective_date <- tryCatch(as.Date(splits_data$effective_date), error = function(e) stop("splits_data$effective_date cannot be coerced to Date"))
  }

  # Sort financial data and splits by date
  financial_data <- dplyr::arrange(financial_data, date)
  splits_data <- dplyr::arrange(splits_data, effective_date)

  # Start with unadjusted shares in the new column
  result <- financial_data %>%
    dplyr::mutate(
      shares_equiv_current_shares = shares_outstanding
    )

    # Apply each split sequentially
    for (i in 1:nrow(splits_data)) {
      split_date <- splits_data$effective_date[i]
      split_factor <- splits_data$split_factor[i]

      # For dates strictly *before* this split, multiply the adjusted shares
      # by the split factor to make them equivalent to post-split shares.
      result <- result %>%
        dplyr::mutate(
          shares_equiv_current_shares = ifelse(
            date < split_date,
            shares_equiv_current_shares * split_factor,
            shares_equiv_current_shares
          )
        )
    }

    return(result)
  }
