#' Make a request to the Alpha Vantage API for delayed quotes
#'
#' @param ticker Character string of the stock ticker symbol
#' @param api_key Character string containing the Alpha Vantage API key
#' @return List containing the price and timestamp
#'
fetch_delayed_quote <- function(ticker, api_key) {
  response <- httr2::request("https://www.alphavantage.co/query") %>%
    httr2::req_url_query(
      "function" = "GLOBAL_QUOTE",
      symbol = ticker,
      apikey = api_key
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  if (!is.null(response[["Global Quote"]])) {
    quote_data <- response[["Global Quote"]]
    return(list(
      price = quote_data[["05. price"]],
      timestamp = quote_data[["07. latest trading day"]]
    ))
  }
  
  return(NULL)
}