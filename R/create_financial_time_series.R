#' Create a Combined Financial Time Series Data Frame
#'
#' Fetches income statement and balance sheet data for a given ticker,
#' adjusts shares outstanding for historical splits, calculates key financial
#' metrics, including per-share metrics based on split-adjusted shares.
#'
#' @param ticker The stock ticker symbol (e.g., "AAPL").
#' @param api_key Your Alpha Vantage API key.
#' @param period "quarterly" (default) or "annual".
#'
#' @return A tibble containing the combined time series of financial data,
#'   including original shares outstanding (`shares_outstanding`), split-adjusted
#'   shares (`shares_equiv_current_shares`), and various calculated metrics.
#'   Returns an empty tibble if data fetching fails for either income or balance sheet.
#' @export
#' @importFrom dplyr full_join arrange mutate select %>%
#' @importFrom rlang .data
create_financial_time_series <- function(ticker, api_key, period = "quarterly") {
  income_data <- fetch_income_statement(ticker, api_key, period)
  if (is.null(income_data) || nrow(income_data) == 0) {
      warning("Failed to fetch or received empty income statement data for ", ticker)
      return(tibble::tibble())
  }
  Sys.sleep(1)
  balance_data <- fetch_balance_sheet(ticker, api_key, period)
   if (is.null(balance_data) || nrow(balance_data) == 0) {
      warning("Failed to fetch or received empty balance sheet data for ", ticker)
      return(tibble::tibble())
  }
  Sys.sleep(1)
  splits_data <- fetch_split_history(ticker, api_key)
  if (is.null(splits_data)) {
      splits_data <- tibble::tibble(effective_date = as.Date(character()), split_factor = numeric())
  }

  balance_data_adjusted <- adjust_shares_for_splits(balance_data, splits_data)

  financial_ts <- income_data %>%
    dplyr::full_join(balance_data_adjusted, by = c("date", "period")) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      gross_margin = ifelse(revenue == 0, 0, gross_profit / revenue),
      operating_margin = ifelse(revenue == 0, 0, operating_income / revenue),
      net_margin = ifelse(revenue == 0, 0, net_income / revenue),
      roe = ifelse(total_equity == 0, 0, net_income / total_equity),
      debt_to_equity = ifelse(total_equity == 0, NA_real_, total_liabilities / total_equity),
      asset_turnover = ifelse(total_assets == 0, 0, revenue / total_assets),
      roa = ifelse(total_assets == 0, 0, net_income / total_assets),
      eps_split_adjusted = ifelse(shares_equiv_current_shares == 0, 0, net_income / shares_equiv_current_shares),
      book_value_per_share_split_adjusted = ifelse(shares_equiv_current_shares == 0, 0, total_equity / shares_equiv_current_shares)
    ) %>%
     dplyr::select(
        date, period,
        revenue, gross_profit, operating_income, net_income, ebit,
        total_assets, total_liabilities, total_equity, shares_outstanding,
        shares_equiv_current_shares,
        gross_margin, operating_margin, net_margin, roe, debt_to_equity, asset_turnover, roa,
        eps_split_adjusted, book_value_per_share_split_adjusted,
        dplyr::everything()
    )

  return(financial_ts)
}
