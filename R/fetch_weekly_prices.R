#' Fetch Weekly Stock Prices with Both Adjusted and Unadjusted Values
#'
#' @param ticker Character string of the stock ticker symbol
#' @param api_key Character string containing the Alpha Vantage API key
#' @return Data frame containing dates and both adjusted and unadjusted closing prices
#'
fetch_weekly_prices <- function(ticker, api_key) {
  # Input validation
  if (!is.character(ticker) || length(ticker) != 1) {
    stop(paste0("ticker must be a single character string, got: ", typeof(ticker)))
  }

  if (!is.character(api_key) || length(api_key) != 1) {
    stop(paste0("api_key must be a single character string, got: ", typeof(api_key)))
  }

  # Get weekly data
  weekly_response <- make_alpha_vantage_request(
    function_name = "TIME_SERIES_WEEKLY_ADJUSTED",
    ticker = ticker,
    api_key = api_key
  )

  # Validate weekly response
  weekly_data <- weekly_response[["Weekly Adjusted Time Series"]]
  if (is.null(weekly_data)) {
    stop(paste0("No weekly price data returned for ticker: ", ticker))
  }

  # Process weekly data
  price_data <- lapply(names(weekly_data), function(date) {
    data <- weekly_data[[date]]
    list(
      date = date,
      close = data[["4. close"]],          # Regular close
      adjusted_close = data[["5. adjusted close"]]  # Split/dividend adjusted close
    )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      close = as.numeric(close),
      adjusted_close = as.numeric(adjusted_close),
      date = lubridate::as_date(date)
    ) %>%
    dplyr::arrange(date)

  return(price_data)
}
