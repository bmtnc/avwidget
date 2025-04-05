#' Create a Financial Metrics Table
#'
#' Takes financial data and creates a formatted GT table with key metrics displayed in two columns.
#' The function handles formatting of currency, percentages and other numeric values.
#'
#' @param data A list containing financial metrics with the following required elements:
#'   MarketCapitalization, RevenueTTM, PERatio, EBITDA, ForwardPE, GrossProfitTTM,
#'   PEGRatio, OperatingMarginTTM, PriceToBookRatio, ProfitMargin, PriceToSalesRatioTTM,
#'   ReturnOnEquityTTM, Beta, ReturnOnAssetsTTM, DividendYield, EPS
#'
#' @return A GT table object containing formatted financial metrics
#'
#' @importFrom tibble tribble
#' @importFrom scales dollar percent
#' @importFrom gt gt tab_header cols_align tab_style tab_options cols_label
create_financial_metrics_table <- function(data) {
  # Input validation
  if (!is.list(data)) {
    stop(paste0("Expected data to be a list, but received: ", class(data)))
  }

  required_fields <- c("MarketCapitalization", "RevenueTTM", "PERatio", "EBITDA", "ForwardPE",
                       "GrossProfitTTM", "PEGRatio", "OperatingMarginTTM", "PriceToBookRatio",
                       "ProfitMargin", "PriceToSalesRatioTTM", "ReturnOnEquityTTM", "Beta",
                       "ReturnOnAssetsTTM", "DividendYield", "EPS")

  missing_fields <- setdiff(required_fields, names(data))
  if (length(missing_fields) > 0) {
    stop(paste0("Missing required fields in data: ", paste(missing_fields, collapse = ", ")))
  }

  metrics_df <- tibble::tribble(
    ~Metric1, ~Value1, ~Metric2, ~Value2,
    "Market Cap", scales::dollar(as.numeric(data$MarketCapitalization), scale = 1e-9, suffix = "B"),
    "Revenue TTM", scales::dollar(as.numeric(data$RevenueTTM), scale = 1e-9, suffix = "B"),
    "P/E Ratio", data$PERatio,
    "EBITDA", scales::dollar(as.numeric(data$EBITDA), scale = 1e-9, suffix = "B"),
    "Forward P/E", data$ForwardPE,
    "Gross Profit TTM", scales::dollar(as.numeric(data$GrossProfitTTM), scale = 1e-9, suffix = "B"),
    "PEG Ratio", data$PEGRatio,
    "Operating Margin", scales::percent(as.numeric(data$OperatingMarginTTM), accuracy = 0.01),
    "Price/Book", data$PriceToBookRatio,
    "Profit Margin", scales::percent(as.numeric(data$ProfitMargin), accuracy = 0.01),
    "Price/Sales", data$PriceToSalesRatioTTM,
    "ROE", scales::percent(as.numeric(data$ReturnOnEquityTTM), accuracy = 0.01),
    "Beta", data$Beta,
    "ROA", scales::percent(as.numeric(data$ReturnOnAssetsTTM), accuracy = 0.01),
    "Dividend Yield", scales::percent(as.numeric(data$DividendYield), accuracy = 0.01),
    "EPS", scales::dollar(as.numeric(data$EPS))
  )

  gt::gt(metrics_df) %>%
    gt::tab_header(title = "Key Metrics") %>%
    gt::cols_align(align = "left", columns = c("Metric1", "Metric2")) %>%
    gt::cols_align(align = "right", columns = c("Value1", "Value2")) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_options(
      table.width = gt::pct(100),
      column_labels.font.weight = "bold",
      table.font.size = "small"
    ) %>%
    gt::cols_label(
      Metric1 = "Metric",
      Value1 = "Value",
      Metric2 = "Metric",
      Value2 = "Value"
    )
}
