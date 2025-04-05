#' Create a Market Cap History Chart
#'
#' Creates a line chart showing historical market capitalization
#'
#' @param df A data frame containing market cap data
#' @param ticker A character string representing the stock ticker symbol
#' @param start_date Start date for the chart
#'
#' @return A ggplot2 object containing the market cap chart
#'
create_market_cap_chart <- function(df, ticker, start_date = "2018-01-01") {
  # Input validation
  if (!is.data.frame(df)) {
    stop(paste0("Expected df to be a data frame, but received: ", class(df)))
  }

  required_cols <- c("date", "market_cap_billions")
  if (!all(required_cols %in% names(df))) {
    stop(paste0("df must contain columns: ", paste(required_cols, collapse = ", "),
                ". Found columns: ", paste(names(df), collapse = ", ")))
  }

  # Filter data by start_date
  df_filtered <- df %>%
    dplyr::filter(date >= lubridate::as_date(start_date))

  # Get latest value for annotation
  latest_date <- max(df_filtered$date)
  latest_mcap <- df_filtered$market_cap_billions[which.max(df_filtered$date)]

  # Create annotation text
  mcap_annotation <- base::paste0(
    "$", base::format(base::round(latest_mcap, 1), big.mark = ","), "B"
  )

  ggplot2::ggplot(df_filtered) +
    # Market cap line
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = market_cap_billions),
      color = "black",
      linewidth = 0.5
    ) +
    # End point dot
    ggplot2::geom_point(
      data = df_filtered[df_filtered$date == latest_date, ],
      ggplot2::aes(x = date, y = market_cap_billions),
      color = "black",
      size = 2
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 20, r = 50, b = 10, l = 10)
    ) +
    ggplot2::labs(
      title = paste(ticker, "Market Cap"),
      x = "",
      y = ""
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::dollar_format(suffix = "B"),
      expand = ggplot2::expansion(mult = c(0.05, 0.2))
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.2))
    ) +
    # Market cap annotation
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = latest_mcap,
      label = mcap_annotation,
      hjust = -0.5,
      vjust = 0.5,
      size = 2.5
    )
}
