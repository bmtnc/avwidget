#' Make a request to the Alpha Vantage API for latest intraday price
#'
#' @param ticker Character string of the stock ticker symbol
#' @param api_key Character string containing the Alpha Vantage API key
#' @return List containing the latest price data
#'
fetch_latest_price <- function(ticker, api_key) {
  response <- make_alpha_vantage_request(
    function_name = "TIME_SERIES_INTRADAY",
    ticker = ticker,
    api_key = api_key,
    interval = "5min",
    outputsize = "compact"
  )

  # Get the time series data
  time_series <- response[["Time Series (5min)"]]

  # Get the most recent timestamp and data
  if (!is.null(time_series) && length(time_series) > 0) {
    latest_timestamp <- names(time_series)[1]  # First entry is most recent
    latest_data <- time_series[[latest_timestamp]]

    return(list(
      timestamp = latest_timestamp,
      price = latest_data[["4. close"]]
    ))
  }

  return(NULL)
}
