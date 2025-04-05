#' Search for Stock Ticker Symbols
#'
#' Uses Alpha Vantage's SYMBOL_SEARCH endpoint to find matching ticker symbols
#' based on keywords.
#'
#' @param keywords Character string to search for (e.g., company name or ticker)
#' @param api_key Character string containing the Alpha Vantage API key
#' @param max_results Integer maximum number of results to return (default: 5)
#' @return A tibble containing matching symbols and their information
#'
#' @importFrom dplyr slice arrange desc
#' @importFrom tibble as_tibble
#' @importFrom httr2 request req_url_query req_perform resp_body_json


search_ticker <- function(keywords, api_key = Sys.getenv("ALPHA_VANTAGE_API_KEY"), 
                         max_results = 5) {
  # Input validation
  if (!is.character(keywords) || length(keywords) != 1) {
    stop("keywords must be a single character string")
  }
  
  if (nchar(keywords) < 1) {
    stop("keywords must not be empty")
  }
  
  if (!is.character(api_key) || nchar(api_key) < 10) {
    stop("Invalid API key")
  }
  
  # Make API request
  tryCatch({
    response <- httr2::request("https://www.alphavantage.co/query") %>%
      httr2::req_url_query(
        "function" = "SYMBOL_SEARCH",
        keywords = keywords,
        apikey = api_key
      ) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
    
    # Check if we got any results
    if (is.null(response$bestMatches) || length(response$bestMatches) == 0) {
      message("No matches found for: ", keywords)
      return(tibble::tibble())
    }
    
    # Process results
    results <- response$bestMatches %>%
      lapply(function(x) {
        list(
          symbol = x[["1. symbol"]],
          name = x[["2. name"]],
          type = x[["3. type"]],
          region = x[["4. region"]],
          market_open = x[["5. marketOpen"]],
          market_close = x[["6. marketClose"]],
          timezone = x[["7. timezone"]],
          currency = x[["8. currency"]],
          match_score = as.numeric(x[["9. matchScore"]])
        )
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(desc(match_score)) %>%
      dplyr::slice(1:min(max_results, n()))
    
    return(results)
    
  }, error = function(e) {
    stop(paste0("API request failed: ", e$message))
  })
}