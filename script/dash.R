# R script for debugging AMZN market cap calculation issues
# This script fetches and displays the same data as dashboard.Rmd
# but in a script format for easier debugging

library(httr2)
library(dplyr)
library(scales)
library(ggplot2)
library(lubridate)
library(tibble)
library(tidyr)
library(gridExtra)

# Set custom theme for all plots
ggplot2::theme_set(
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "gray90"),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      legend.position = "bottom"
    )
)

# Load package functions
devtools::load_all()

# Configuration
ticker <- "AMZN"
start_date <- "2018-01-01"
api_key <- Sys.getenv("ALPHA_VANTAGE_API_KEY")
if (api_key == "") stop("ALPHA_VANTAGE_API_KEY environment variable is not set")

# Fetch company overview
fetch_company_overview <- function(ticker, api_key) {
  make_alpha_vantage_request("OVERVIEW", ticker, api_key)
}

# Step 1: Company Overview
company_data <- fetch_company_overview(ticker, api_key)
cat("\nCompany Overview:\n")
cat("Name:", company_data$Name, "\n")
cat("Symbol:", company_data$Symbol, "\n")
cat("Exchange:", company_data$Exchange, "\n")
cat("Sector:", company_data$Sector, "\n")
cat("Industry:", company_data$Industry, "\n")
Sys.sleep(1)  # 1 second delay

# Step 2: Fetch price data
raw_price_data <- fetch_weekly_prices(ticker, api_key)
price_data <- raw_price_data %>%
  dplyr::select(date, close, adjusted_close)

cat("\nPrice Data Summary:\n")
cat("Date Range:", min(price_data$date), "to", max(price_data$date), "\n")
cat("Recent Prices:\n")
print(tail(price_data, 5))
Sys.sleep(1)  # 1 second delay

# Step 3: Fetch financial data
financial_data <- create_financial_time_series(ticker, api_key, period = "quarterly")

cat("\nFinancial Data Summary:\n")
cat("Date Range:", min(financial_data$date), "to", max(financial_data$date), "\n")
cat("Financial Data Columns:", paste(names(financial_data), collapse=", "), "\n")
cat("Recent Financial Data:\n")
print(tail(select(financial_data, date, revenue, ebit, shares_outstanding, shares_outstanding_adjusted), 5))

# Step 4: Fetch split history separately for inspection
splits_data <- fetch_split_history(ticker, api_key)
cat("\nStock Split History:\n")
if (nrow(splits_data) > 0) {
  print(splits_data)
} else {
  cat("No splits found in data\n")
}

# Step 5: Calculate TTM metrics
financial_data <- tryCatch({
  calculate_ttm_metrics(financial_data)
}, error = function(e) {
  warning("TTM calculation failed: ", e$message)
  financial_data  # return original data if TTM calc fails
})

cat("\nTTM Metrics Added:\n")
ttm_cols <- names(financial_data)[grepl("_ttm", names(financial_data))]
cat("TTM Columns:", paste(ttm_cols, collapse=", "), "\n")

# Step 6: Calculate historical market cap
market_cap_history <- calculate_historical_market_cap(price_data, financial_data)

cat("\nMarket Cap History:\n")
cat("Date Range:", min(market_cap_history$date), "to", max(market_cap_history$date), "\n")
cat("Market Cap Range:",
    scales::dollar(min(market_cap_history$market_cap_billions, na.rm=TRUE), suffix="B"), "to",
    scales::dollar(max(market_cap_history$market_cap_billions, na.rm=TRUE), suffix="B"), "\n")
cat("Recent Market Cap Data:\n")
print(tail(select(market_cap_history, date, close, shares_outstanding_adjusted, market_cap_billions), 10))

# Step 7: Debug specific periods around splits
if (nrow(splits_data) > 0) {
  cat("\nDetailed Market Cap Around Split Dates:\n")
  for (i in 1:nrow(splits_data)) {
    split_date <- splits_data$effective_date[i]
    window_start <- split_date - days(30)
    window_end <- split_date + days(30)

    cat("\nSplit on", split_date, "with factor", splits_data$split_factor[i], ":\n")
    before_after_data <- market_cap_history %>%
      filter(date >= window_start & date <= window_end) %>%
      select(date, close, adjusted_close, shares_outstanding_adjusted, market_cap_billions)

    # Show data points before and after split
    print(before_after_data)
  }
}

# Step 8: Save detailed data for further inspection
debug_data <- list(
  company_data = company_data,
  price_data = price_data,
  financial_data = financial_data,
  splits_data = splits_data,
  market_cap_history = market_cap_history
)


# Step 9: Generate diagnostic plots

# Price chart
price_chart <- create_price_chart(price_data, ticker, start_date = start_date)
print(price_chart)

# Market cap chart
market_cap_chart <- create_market_cap_chart(market_cap_history, ticker)
print(market_cap_chart)

# Create diagnostic chart showing shares outstanding
shares_plot <- ggplot(financial_data, aes(x = date)) +
  geom_line(aes(y = shares_outstanding, color = "Original Shares")) +
  geom_line(aes(y = shares_outstanding_adjusted, color = "Adjusted Shares")) +
  scale_color_manual(values = c("Original Shares" = "blue", "Adjusted Shares" = "red")) +
  labs(title = paste(ticker, "- Shares Outstanding Comparison"),
       y = "Shares (Millions)",
       x = "Date",
       color = "") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6))

print(shares_plot)

cat("\nScript execution complete\n")
