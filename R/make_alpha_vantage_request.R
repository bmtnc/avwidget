#' Make a request to the Alpha Vantage API
#'
#' Sends a request to the Alpha Vantage API with the specified function name, ticker and API key.
#' Handles errors and returns the JSON response body.
#'
#' @param function_name Character string specifying the Alpha Vantage API function to call
#' @param ticker Character string of the stock ticker symbol
#' @param api_key Character string containing the Alpha Vantage API key
#' @param ... Additional parameters to pass to the API request
#' @return List containing the JSON response from the API
#' @importFrom httr2 request req_url_query req_perform resp_body_json resp_status
#'
#
make_alpha_vantage_request <- function(function_name, ticker, api_key, ...) {
  # Input validation
  if (!is.character(function_name) || length(function_name) != 1) {
    stop(paste0("function_name must be a single character string, got: ", typeof(function_name)))
  }

  if (!is.character(ticker) || length(ticker) != 1) {
    stop(paste0("ticker must be a single character string, got: ", typeof(ticker)))
  }

  if (!is.character(api_key) || length(api_key) != 1) {
    stop(paste0("api_key must be a single character string, got: ", typeof(api_key)))
  }

  if (nchar(api_key) < 10) {
    stop(paste0("api_key appears invalid - too short: ", nchar(api_key), " characters"))
  }

  tryCatch({
    response <- httr2::request("https://www.alphavantage.co/query") %>%
      httr2::req_url_query(
        "function" = function_name,
        symbol = ticker,
        apikey = api_key,
        ...
      ) %>%
      httr2::req_perform()

    httr2::resp_body_json(response)
  },
  error = function(e) {
    stop(paste0(
      "Alpha Vantage API call failed for ticker: ", ticker,
      ", function: ", function_name,
      ". Error: ", e$message
    ))
  })
}
