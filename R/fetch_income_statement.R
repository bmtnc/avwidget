#' Fetch and Process Income Statement Data
#'
#' @param ticker Character string of the stock ticker symbol
#' @param api_key Character string containing the Alpha Vantage API key
#' @param period Character string either "annual" or "quarterly"
#' @return A tibble with processed income statement data
#'
fetch_income_statement <- function(ticker, api_key, period = "quarterly") {
  response <- make_alpha_vantage_request(
    function_name = "INCOME_STATEMENT",
    ticker = ticker,
    api_key = api_key
  )

  # Select the appropriate report type
  reports <- if(period == "quarterly") {
    response$quarterlyReports
  } else {
    response$annualReports
  }

  # Convert to tibble and process
  income_df <- reports %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      across(where(is.character), ~ifelse(. == "None", NA, .)),
      across(-c(fiscalDateEnding, reportedCurrency), as.numeric),
      fiscalDateEnding = lubridate::ymd(fiscalDateEnding)
    ) %>%
    dplyr::select(
      fiscalDateEnding,
      date = fiscalDateEnding,
      revenue = totalRevenue,
      gross_profit = grossProfit,
      operating_income = operatingIncome,
      net_income = netIncome,
      ebit = ebit,
      ebitda = ebitda,
      rd_expense = researchAndDevelopment,
      sga_expense = sellingGeneralAndAdministrative
    ) %>%
    dplyr::mutate(period = period) %>%
    dplyr::arrange(desc(date))

  return(income_df)
}