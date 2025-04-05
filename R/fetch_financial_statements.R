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
    dplyr::arrange(desc(date))

  return(income_df)
}

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
      date = fiscalDateEnding,
      total_assets = totalAssets,
      total_liabilities = totalLiabilities,
      total_equity = totalShareholderEquity,
      cash = cashAndCashEquivalentsAtCarryingValue,
      long_term_debt = longTermDebt,
      retained_earnings = retainedEarnings,
      shares_outstanding = commonStockSharesOutstanding
    ) %>%
    dplyr::arrange(desc(date))

  return(balance_df)
}

create_financial_time_series <- function(ticker, api_key, period = "quarterly") {
  # Fetch original data
  income_data <- fetch_income_statement(ticker, api_key, period)
  Sys.sleep(1)  # Rate limit compliance
  balance_data <- fetch_balance_sheet(ticker, api_key, period)
  Sys.sleep(1)  # Rate limit compliance

  # Fetch split history
  splits_data <- fetch_split_history(ticker, api_key)

  # Adjust balance sheet data for splits
  balance_data_adjusted <- adjust_shares_for_splits(balance_data, splits_data)

  # Combine datasets using adjusted balance sheet data
  financial_ts <- income_data %>%
    dplyr::full_join(balance_data_adjusted, by = "date") %>%
    dplyr::arrange(date) %>%
    # Calculate additional metrics using adjusted shares
    dplyr::mutate(
      gross_margin = gross_profit / revenue,
      operating_margin = operating_income / revenue,
      net_margin = net_income / revenue,
      roe = net_income / total_equity,
      debt_to_equity = total_liabilities / total_equity,
      asset_turnover = revenue / total_assets,
      roa = net_income / total_assets,
      # Add per-share metrics using adjusted shares
      eps = net_income / shares_outstanding_adjusted,
      book_value_per_share = total_equity / shares_outstanding_adjusted
    )

  return(financial_ts)
}
