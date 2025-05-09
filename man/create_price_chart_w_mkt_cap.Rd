% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_price_chart_w_mkt_cap.R
\name{create_price_chart_w_mkt_cap}
\alias{create_price_chart_w_mkt_cap}
\title{Create a Stock Price History Chart with Price and Market Cap Percentage Changes}
\usage{
create_price_chart_w_mkt_cap(df, ticker, start_date = "2018-01-01")
}
\arguments{
\item{df}{A data frame containing stock price and market cap data}

\item{ticker}{A character string representing the stock ticker symbol}

\item{start_date}{Start date for the chart}
}
\value{
A ggplot2 object containing the price history chart
}
\description{
Creates a line chart showing cumulative percentage changes for price and market cap
}
