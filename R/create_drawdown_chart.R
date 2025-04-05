#' Create a Stock Price Drawdown Chart
#'
#' Creates a line chart showing historical drawdown from all-time highs
#' starting from 2007
#'
#' @param df A data frame containing stock price data with columns 'date' and 'adjusted_close'
#' @param ticker A character string representing the stock ticker symbol
#'
#' @return A ggplot2 object containing the drawdown history chart
#'
#' @importFrom ggplot2 ggplot aes geom_line theme labs scale_y_continuous scale_x_date expansion
#' @importFrom scales percent_format
create_drawdown_chart <- function(df, ticker) {
  # Input validation
  if (!is.data.frame(df)) {
    stop(paste0("Expected df to be a data frame, but received: ", class(df)))
  }

  if (!all(c("date", "adjusted_close") %in% names(df))) {
    stop(paste0("df must contain 'date' and 'adjusted_close' columns. Found columns: ",
                paste(names(df), collapse = ", ")))
  }

  # Filter data to start from 2007
  df_filtered <- df %>%
    dplyr::filter(date > lubridate::ymd("2006-12-31"))

  # Ensure we have data after filtering
  if (nrow(df_filtered) == 0) {
    stop("No data available after 2006-12-31")
  }

  # Calculate rolling maximum and drawdown
  df_filtered <- df_filtered %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      rolling_max = cummax(adjusted_close),
      drawdown = (adjusted_close - rolling_max) / rolling_max
    )

  # Get current drawdown for annotation
  current_drawdown <- df_filtered$drawdown[which.max(df_filtered$date)]
  latest_date <- max(df_filtered$date)

  ggplot2::ggplot(df_filtered, ggplot2::aes(x = date, y = drawdown)) +
    # Base line
    ggplot2::geom_line(color = "#A42D2B", linewidth = 0.5) +
    # Zero reference line
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.3) +
    # End point dot
    ggplot2::geom_point(
      data = df_filtered[df_filtered$date == latest_date, ],
      color = "#A42D2B",
      size = 2
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 20, r = 50, b = 10, l = 10)
    ) +
    ggplot2::labs(
      title = paste(ticker, "Drawdown"),
      x = "",
      y = ""
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      expand = ggplot2::expansion(mult = c(0.1, 0.1))
    ) +
    ggplot2::scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.1))
    ) +
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = current_drawdown,
      label = scales::percent(current_drawdown, accuracy = 0.1),
      hjust = -0.5,
      vjust = 0.5,
      size = 2.5
    )
}