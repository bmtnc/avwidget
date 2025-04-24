#' Fetch and Process Balance Sheet Data
#'
#' @param ticker Character string of the stock ticker symbol
#' @param api_key Character string containing the Alpha Vantage API key
#' @param period Character string either "annual" or "quarterly"
#' @return A tibble with processed balance sheet data
#'
fetch_balance_sheet <- function(ticker, api_key, period = "quarterly") {
  response <- make_alpha_vantage_request(
    function_name = "BALANCE_SHEET",
    ticker = ticker,
    api_key = api_key
  )

  reports <- if(period == "quarterly") {
    response$quarterlyReports
  } else {
    response$annualReports
  }

  balance_df <- reports %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      across(where(is.character), ~ifelse(. == "None", NA, .)),
      across(-c(fiscalDateEnding, reportedCurrency), as.numeric),
      fiscalDateEnding = lubridate::ymd(fiscalDateEnding)
    ) %>%
    dplyr::select(
      fiscalDateEnding,
      date = fiscalDateEnding,
      total_assets = totalAssets,
      total_liabilities = totalLiabilities,
      total_equity = totalShareholderEquity,
      cash = cashAndCashEquivalentsAtCarryingValue,
      long_term_debt = longTermDebt,
      retained_earnings = retainedEarnings,
      shares_outstanding = commonStockSharesOutstanding
    ) %>%
    dplyr::mutate(period = period) %>%
    dplyr::arrange(desc(date))

  return(balance_df)
}