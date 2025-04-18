% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_historical_market_cap.R
\name{calculate_historical_market_cap}
\alias{calculate_historical_market_cap}
\title{Calculate Historical Market Capitalization}
\usage{
calculate_historical_market_cap(price_data, financial_data)
}
\arguments{
\item{price_data}{A dataframe containing daily/weekly price data with date, adjusted_close, and adjusted_adjusted_close columns}

\item{financial_data}{A dataframe containing quarterly financial data including shares_outstanding_adjusted}
}
\value{
A dataframe with historical market cap calculations
}
\description{
Combines price data with split-adjusted shares outstanding to calculate historical market cap.
Uses unadjusted adjusted_close price since share count is already split-adjusted.
}
