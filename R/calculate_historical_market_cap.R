#' Calculate Historical Market Capitalization
#'
#' Calculates historical market capitalization by combining price data and
#' financial time series data. It uses adjusted closing prices and
#' split-adjusted equivalent shares outstanding.
#'
#' @param price_data A dataframe containing at least 'date' and 'adjusted_close' columns.
#' @param financial_ts A dataframe containing at least 'date' and 'shares_outstanding' columns.
#'
#' @return A tibble with 'date', 'adjusted_close', 'shares_outstanding',
#'   and 'market_cap'. Returns empty tibble if inputs are insufficient.
#' @export

calculate_historical_market_cap <- function(price_data, financial_ts) {
  empty_result <- tibble::tibble(
    date = as.Date(character()),
    adjusted_close = numeric(),
    shares_outstanding = numeric(),
    market_cap = numeric()
  )

  # Input Validation
  if (!is.data.frame(price_data) || !all(c("date", "adjusted_close") %in% names(price_data))) {
    warning("Invalid 'price_data'. Must be a data frame with 'date' and 'adjusted_close' columns.")
    return(empty_result)
  }

  if (!is.data.frame(financial_ts) || !all(c("date", "shares_outstanding") %in% names(financial_ts))) {
    warning("Invalid 'financial_ts'. Must be a data frame with 'date' and 'shares_outstanding' columns.")
    return(empty_result)
  }

  # Return empty result for empty inputs (no warning)
  if (nrow(price_data) == 0 || nrow(financial_ts) == 0) {
    return(empty_result)
  }

  # Ensure date columns are Date type
  if (!lubridate::is.Date(price_data$date)) {
    tryCatch({
      price_data <- price_data %>% dplyr::mutate(date = as.Date(date))
    }, error = function(e) {
      stop("Failed to convert price_data$date to Date type.")
    })
  }

  if (!lubridate::is.Date(financial_ts$date)) {
    tryCatch({
      financial_ts <- financial_ts %>% dplyr::mutate(date = as.Date(date))
    }, error = function(e) {
      stop("Failed to convert financial_ts$date to Date type.")
    })
  }

  # Prepare data
  price_subset <- price_data %>%
    dplyr::select(date, adjusted_close) %>%
    dplyr::arrange(date)

  financials_subset <- financial_ts %>%
    dplyr::select(date, shares_outstanding) %>%
    dplyr::arrange(date)

  # Filter price data to only include dates >= earliest financial date
  min_financial_date <- min(financials_subset$date)
  price_subset <- price_subset %>%
    dplyr::filter(date >= min_financial_date)

  if (nrow(price_subset) == 0) {
    return(empty_result)  # No price data on or after the first financial date
  }

  # Create a comprehensive financial dataset with forward fill
  # First, get all unique dates from both datasets
  all_dates <- unique(c(price_subset$date, financials_subset$date))
  all_dates <- sort(all_dates)

  # Create a complete timeline
  full_timeline <- tibble::tibble(date = all_dates)

  # Join financial data to the timeline and forward fill
  filled_financials <- full_timeline %>%
    dplyr::left_join(financials_subset, by = "date") %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      shares_outstanding = zoo::na.locf(shares_outstanding, na.rm = FALSE)
    )

  # Now join with price data
  result_data <- price_subset %>%
    dplyr::left_join(
      filled_financials %>% dplyr::select(date, shares_outstanding),
      by = "date"
    ) %>%
    dplyr::arrange(date)

  # Drop rows with NA values (where no share count is available)
  result_data <- result_data %>%
    dplyr::filter(!is.na(shares_outstanding))

  # Calculate market cap
  result_data <- result_data %>%
    dplyr::mutate(
      market_cap = adjusted_close * shares_outstanding
    )

  return(result_data)
}
