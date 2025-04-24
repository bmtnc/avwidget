# script/single_stock_analysis.R
# Script to prepare all data for dashboard rendering
# This script creates all data objects needed for visualization
# and saves them to be loaded by dashboard.Rmd

# Parameters (could be command line args in future)
ticker <- "SMRT"
latest_price <- NULL
latest_price_date <- Sys.Date()

# Validate API key
api_key <- Sys.getenv("ALPHA_VANTAGE_API_KEY")
if (api_key == "") stop("ALPHA_VANTAGE_API_KEY environment variable is not set")

# Fetch company overview
company_data <- make_alpha_vantage_request("OVERVIEW", ticker, api_key)
Sys.sleep(1) # API rate limiting

# Fetch price data
raw_price_data <- fetch_weekly_prices(ticker, api_key)
Sys.sleep(1)

# Process price data
price_data <- raw_price_data %>%
  dplyr::select(date, close, adjusted_close)

# Handle manual price override if provided
if (!is.null(latest_price)) {
  latest_row <- data.frame(
    date = latest_price_date,
    close = latest_price,
    adjusted_close = latest_price
  )

  price_data <- price_data %>%
    dplyr::filter(date < latest_price_date) %>%
    dplyr::bind_rows(latest_row) %>%
    dplyr::arrange(date)
}

# Fetch financial data
financial_data <- create_financial_time_series(ticker, api_key, period = "quarterly")

# Validate quarterly data continuity
tryCatch({
  check_continuous_quarterly_data(financial_data)
}, error = function(e) {
  warning("Data continuity check failed: ", e$message)
})

# Calculate TTM metrics
financial_data <- tryCatch({
  calculate_ttm_metrics(financial_data)
}, error = function(e) {
  warning("TTM calculation failed: ", e$message)
  financial_data
})

# Determine latest quarter for transcript
current_quarter <- lubridate::quarter(Sys.Date())
current_year <- lubridate::year(Sys.Date())
latest_quarter <- if(current_quarter == 1) {
  sprintf("%dQ4", current_year - 1)
} else {
  sprintf("%dQ%d", current_year, current_quarter - 1)
}

# Fetch earnings transcript
transcript_result <- tryCatch({
  fetch_transcript_text(ticker, latest_quarter, api_key)
}, error = function(e) {
  warning("Transcript fetch failed: ", e$message)
  NULL
})

# Calculate market cap history
market_cap_history <- calculate_historical_market_cap(price_data, financial_data)

# Generate enhanced description
enhanced_description <- tryCatch({
  generate_combined_description(
    alpha_vantage_desc = company_data$Description,
    transcript_result = transcript_result,
    ticker = ticker
  )
}, error = function(e) {
  warning("Enhanced description generation failed: ", e$message)
  company_data$Description
})

# Analyze secular trends
secular_trends <- tryCatch({
  analyze_secular_trends(
    enhanced_description = enhanced_description,
    ticker = ticker
  )
}, error = function(e) {
  warning("Secular trends analysis failed: ", e$message)
  NULL
})

# Analyze earnings call
earnings_analysis <- if (!is.null(transcript_result)) {
  tryCatch({
    analyze_earnings_call(transcript_result, ticker)
  }, error = function(e) {
    warning("Earnings call analysis failed: ", e$message)
    list(
      fundamentals = "Earnings call analysis currently unavailable.",
      qa = NULL
    )
  })
} else {
  list(
    fundamentals = "Earnings call analysis currently unavailable due to missing transcript.",
    qa = NULL,
    quarter = latest_quarter
  )
}

# Create metrics dataframe
metrics_df <- create_metrics_dataframe(company_data)

# Save the data to a list for use in RMarkdown
list(
  company_data = company_data,
  price_data = price_data,
  financial_data = financial_data,
  market_cap_history = market_cap_history,
  enhanced_description = enhanced_description,
  secular_trends = secular_trends,
  earnings_analysis = earnings_analysis,
  metrics_df = metrics_df
)
