#' Create a Stock Price History Chart with Price and Market Cap Percentage Changes
#'
#' Creates a line chart showing cumulative percentage changes for price and market cap
#'
#' @param df A data frame containing stock price and market cap data
#' @param ticker A character string representing the stock ticker symbol
#' @param start_date Start date for the chart
#'
#' @return A ggplot2 object containing the price history chart
#'
create_price_chart_w_mkt_cap <- function(df, ticker, start_date = "2018-01-01") {
  # Input validation
  if (!is.data.frame(df)) {
    stop(paste0("Expected df to be a data frame, but received: ", class(df)))
  }

  required_cols <- c("date", "adjusted_close", "market_cap_billions")
  if (!all(required_cols %in% names(df))) {
    stop(paste0("df must contain columns: ", paste(required_cols, collapse = ", "), 
                ". Found columns: ", paste(names(df), collapse = ", ")))
  }

  # Filter data and calculate cumulative percentage changes
  df_filtered <- df %>%
    dplyr::filter(date >= lubridate::as_date(start_date)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      price_pct = (adjusted_close / first(adjusted_close) - 1) * 100,
      mcap_pct = (market_cap_billions / first(market_cap_billions) - 1) * 100
    )

  # Get latest values for annotations
  latest_date <- max(df_filtered$date)
  latest_price_pct <- df_filtered$price_pct[which.max(df_filtered$date)]
  latest_mcap_pct <- df_filtered$mcap_pct[which.max(df_filtered$date)]
  
  # Create annotation text
  price_annotation <- base::paste0(
    "Price: ", base::format(base::round(latest_price_pct, 1)), "%\n",
    "($", base::format(base::round(df_filtered$adjusted_close[which.max(df_filtered$date)], 2), 
                      big.mark = ","), ")"
  )
  
  mcap_annotation <- base::paste0(
    "Mkt Cap: ", base::format(base::round(latest_mcap_pct, 1)), "%\n",
    "($", base::format(base::round(df_filtered$market_cap_billions[which.max(df_filtered$date)], 1), 
                      big.mark = ","), "B)"
  )

  ggplot2::ggplot(df_filtered) +
    # Price line
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = price_pct),
      color = "black",
      linewidth = 0.5
    ) +
    # Market cap line
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = mcap_pct),
      color = "gray50",
      linetype = "dotted",
      linewidth = 0.5
    ) +
    # End point dots
    ggplot2::geom_point(
      data = df_filtered[df_filtered$date == latest_date, ],
      ggplot2::aes(x = date, y = price_pct),
      color = "black",
      size = 2
    ) +
    ggplot2::geom_point(
      data = df_filtered[df_filtered$date == latest_date, ],
      ggplot2::aes(x = date, y = mcap_pct),
      color = "gray50",
      size = 2
    ) +
    # Reference line at 0%
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray70",
      linewidth = 0.3
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 20, r = 50, b = 10, l = 10)
    ) +
    ggplot2::labs(
      title = ticker,
      x = "",
      y = "Cumulative Change (%)"
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.05, 0.2))
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.2))
    ) +
    # Price annotation
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = latest_price_pct,
      label = price_annotation,
      hjust = -0.5,
      vjust = 1.0,
      size = 2.5
    ) +
    # Market cap annotation
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = latest_mcap_pct,
      label = mcap_annotation,
      hjust = -0.5,
      vjust = -0.2,
      size = 2.5,
      color = "gray50"
    )
}