#' Create Financial Metrics DataFrame
#'
#' Creates a formatted tibble containing financial metrics grouped by type in columns
#'
#' @param data A list containing financial data from Alpha Vantage API
#' @return A tibble with formatted financial metrics
#'
#' @importFrom tibble tribble
#' @importFrom scales dollar percent comma
create_metrics_dataframe <- function(data) {
  if (!is.list(data)) {
    stop(paste0("Expected data to be a list, but received: ", class(data)))
  }

  # Column 1: Valuation Metrics
  val_metrics <- tibble::tribble(
    ~Metric1, ~Value1,
    "Market Cap", scales::dollar(as.numeric(data$MarketCapitalization), scale = 1e-9, suffix = "B"),
    "P/E (TTM)", data$TrailingPE,
    "Forward P/E", data$ForwardPE,
    "EV/EBITDA", data$EVToEBITDA,
    "EV/Revenue", data$EVToRevenue,
    "Price/Sales", data$PriceToSalesRatioTTM,
    "Price/Book", data$PriceToBookRatio
  )

  # Column 2: Financial Metrics
  fin_metrics <- tibble::tribble(
    ~Metric2, ~Value2,
    "Revenue TTM", scales::dollar(as.numeric(data$RevenueTTM), scale = 1e-9, suffix = "B"),
    "Gross Profit", scales::dollar(as.numeric(data$GrossProfitTTM), scale = 1e-9, suffix = "B"),
    "EBITDA", scales::dollar(as.numeric(data$EBITDA), scale = 1e-9, suffix = "B"),
    "Operating Margin", scales::percent(as.numeric(data$OperatingMarginTTM), accuracy = 0.01),
    "Profit Margin", scales::percent(as.numeric(data$ProfitMargin), accuracy = 0.01),
    "ROE", scales::percent(as.numeric(data$ReturnOnEquityTTM), accuracy = 0.01),
    "ROA", scales::percent(as.numeric(data$ReturnOnAssetsTTM), accuracy = 0.01)
  )

  # Column 3: Per Share & Trading Metrics
  share_metrics <- tibble::tribble(
    ~Metric3, ~Value3,
    "EPS (Diluted)", scales::dollar(as.numeric(data$DilutedEPSTTM)),
    "Book Value/Share", scales::dollar(as.numeric(data$BookValue)),
    "Dividend/Share", scales::dollar(as.numeric(data$DividendPerShare)),
    "Dividend Yield", scales::percent(as.numeric(data$DividendYield), accuracy = 0.01),
    "Shares Out.", scales::comma(as.numeric(data$SharesOutstanding), scale = 1e-6, suffix = "M"),
    "52-Week High", scales::dollar(as.numeric(data$`52WeekHigh`)),
    "52-Week Low", scales::dollar(as.numeric(data$`52WeekLow`))
  )

  # Combine all metrics side by side
  dplyr::bind_cols(val_metrics, fin_metrics, share_metrics)
}
