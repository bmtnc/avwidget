#' Create a Stock Price History Chart with Price Callout
#'
#' Creates a line chart showing historical adjusted closing prices with latest price
#'
#' @param df A data frame containing stock price data with columns 'date' and 'adjusted_close'
#' @param ticker A character string representing the stock ticker symbol
#' @param start_date Start date for the chart
#'
#' @return A ggplot2 object containing the price history chart
#'
#' @importFrom ggplot2 ggplot aes geom_line theme labs scale_y_continuous scale_x_date expansion
#' @importFrom scales dollar_format
plot_price <- function(df, ticker, start_date = "2018-01-01") {
  # Input validation
  if (!is.data.frame(df)) {
    stop(paste0("Expected df to be a data frame, but received: ", class(df)))
  }

  if (!all(c("date", "adjusted_close") %in% names(df))) {
    stop(paste0("df must contain 'date' and 'adjusted_close' columns. Found columns: ",
                paste(names(df), collapse = ", ")))
  }

  # Filter data by start_date
  df_filtered <- df %>%
    dplyr::filter(date >= lubridate::as_date(start_date))

  # Get latest price for annotation
  latest_price <- df_filtered$adjusted_close[which.max(df_filtered$date)]
  latest_date <- max(df_filtered$date)

  # Create annotation text
  annotation_text <- paste0("$", format(round(latest_price, 2), big.mark = ","))

  ggplot2::ggplot(df_filtered, ggplot2::aes(x = date, y = adjusted_close)) +
    ggplot2::geom_line(color = "black", linewidth = 0.5) +
    ggplot2::geom_point(
      data = df_filtered[df_filtered$date == latest_date, ],
      color = "black",
      size = 2
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 20, r = 50, b = 10, l = 10)
    ) +
    ggplot2::labs(
      title = ticker,
      x = "",
      y = ""
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::dollar_format(),
      expand = ggplot2::expansion(mult = c(0.05, 0.2))
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.2))
    ) +
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = latest_price,
      label = annotation_text,
      hjust = -0.5,
      vjust = 0.5,
      size = 2.5
    )
}
