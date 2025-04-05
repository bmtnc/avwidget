#' Fetch Stock Split History
#'
#' @param ticker Character string of the stock ticker symbol
#' @param api_key Character string containing the Alpha Vantage API key
#' @return A dataframe of split events with dates and factors
#'
fetch_split_history <- function(ticker, api_key) {
  response <- make_alpha_vantage_request(
    function_name = "SPLITS",
    ticker = ticker,
    api_key = api_key
  )
  
  # Check if response has data
  if (is.null(response$data) || length(response$data) == 0) {
    # Return default dataframe with split factor of 1
    return(data.frame(
      effective_date = as.Date("1900-01-01"),  # Historical date before any likely data
      split_factor = 1,
      stringsAsFactors = FALSE
    ))
  }
  
  # Convert response to dataframe
  splits_df <- response[["data"]] %>%
    lapply(function(x) {
      data.frame(
        effective_date = x[["effective_date"]],
        split_factor = x[["split_factor"]],
        stringsAsFactors = FALSE
      )
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      effective_date = lubridate::as_date(effective_date),
      split_factor = as.numeric(split_factor)
    ) %>%
    dplyr::arrange(effective_date)
  
  return(splits_df)
}