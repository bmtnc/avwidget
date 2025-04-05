#' Calculate Historical Market Capitalization
#'
#' Combines price data with split-adjusted shares outstanding to calculate historical market cap.
#' Uses unadjusted adjusted_close price since share count is already split-adjusted.
#'
#' @param price_data A dataframe containing daily/weekly price data with date, adjusted_close, and adjusted_adjusted_close columns
#' @param financial_data A dataframe containing quarterly financial data including shares_outstanding_adjusted
#' @return A dataframe with historical market cap calculations
#'
#' @importFrom tidyr fill
#' @importFrom dplyr left_join arrange mutate
calculate_historical_market_cap <- function(price_data, financial_data) {
  # Input validation
  if (!all(c("date", "adjusted_close") %in% names(price_data))) {
    stop("price_data must contain 'date', 'adjusted_close' columns")
  }

  if (!all(c("date", "shares_outstanding_adjusted") %in% names(financial_data))) {
    stop("financial_data must contain 'date' and 'shares_outstanding_adjusted' columns")
  }

  # Ensure dates are in Date format
  price_data <- price_data %>%
    dplyr::mutate(date = lubridate::as_date(date))

  financial_data <- financial_data %>%
    dplyr::mutate(date = lubridate::as_date(date))

  # Extract just the shares outstanding data and sort by date
  shares_data <- financial_data %>%
    dplyr::select(date, shares_outstanding_adjusted) %>%
    dplyr::arrange(date)

  # Create a complete date sequence from min to max date
  date_seq <- data.frame(
    date = seq.Date(
      from = min(price_data$date),
      to = max(price_data$date),
      by = "day"
    )
  )

  # Join shares data to complete date sequence and forward fill
  shares_filled <- date_seq %>%
    dplyr::left_join(shares_data, by = "date") %>%
    tidyr::fill(shares_outstanding_adjusted, .direction = "down") %>%
    dplyr::filter(!is.na(shares_outstanding_adjusted))

  # Join with price data and calculate market cap using unadjusted adjusted_close
  market_cap_df <- price_data %>%
    dplyr::left_join(shares_filled, by = "date") %>%
    dplyr::mutate(
      market_cap = adjusted_close * shares_outstanding_adjusted,  # Using unadjusted adjusted_close
      market_cap_billions = market_cap / 1e9
    ) %>%
    dplyr::arrange(date)

  return(market_cap_df)
}
