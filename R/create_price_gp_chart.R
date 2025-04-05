#' Create Price/Gross Profit Multiple Chart
#'
#' Creates a line chart showing historical Price/Gross Profit multiple evolution
#' using TTM Gross Profit values
#'
#' @param market_cap_history DataFrame with daily/weekly market cap data
#' @param financial_data DataFrame with quarterly financial data including gross_profit
#' @param ticker Character string of the stock ticker symbol
#' @param start_date Start date for the chart (default: "2018-01-01")
#' @return A ggplot2 object containing the Price/Gross Profit chart
#'
#' @importFrom ggplot2 ggplot aes geom_line theme labs scale_y_continuous scale_x_date
#' @importFrom tidyr fill
#' @importFrom dplyr left_join mutate arrange lag
create_price_gp_chart <- function(market_cap_history, financial_data,
                                  ticker, start_date = "2018-01-01") {
  # Input validation
  if (!all(c("date", "market_cap") %in% names(market_cap_history))) {
    stop("market_cap_history must contain 'date' and 'market_cap' columns")
  }

  if (!all(c("date", "gross_profit") %in% names(financial_data))) {
    stop("financial_data must contain 'date' and 'gross_profit' columns")
  }

  # Calculate TTM Gross Profit using rolling 4 quarters
  ttm_gp <- financial_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      lag1 = dplyr::lag(gross_profit, 1),
      lag2 = dplyr::lag(gross_profit, 2),
      lag3 = dplyr::lag(gross_profit, 3),
      ttm_gp = (gross_profit + lag1 + lag2 + lag3)
    ) %>%
    dplyr::select(date, ttm_gp)

  # Create date sequence and forward fill TTM Gross Profit
  gp_filled <- data.frame(
    date = seq.Date(
      from = min(market_cap_history$date),
      to = max(market_cap_history$date),
      by = "day"
    )
  ) %>%
    dplyr::left_join(ttm_gp, by = "date") %>%
    tidyr::fill(ttm_gp, .direction = "down") %>%
    dplyr::filter(!is.na(ttm_gp))

  # Calculate Price/Gross Profit ratio
  price_gp_df <- market_cap_history %>%
    dplyr::left_join(gp_filled, by = "date") %>%
    dplyr::mutate(
      price_gp_ratio = market_cap / ttm_gp,
      # Handle potential negative or zero Gross Profit
      price_gp_ratio = ifelse(ttm_gp <= 0, NA, price_gp_ratio)
    ) %>%
    dplyr::filter(
      date >= lubridate::as_date(start_date),
      !is.na(price_gp_ratio)
    )

  # Calculate median for reference line
  median_ratio <- median(price_gp_df$price_gp_ratio, na.rm = TRUE)
  # Calculate current ratio for annotation
  latest_ratio <- price_gp_df$price_gp_ratio[which.max(price_gp_df$date)]
  latest_date <- max(price_gp_df$date)

  # Create callout text
  callout_text <- sprintf("%.1fx", latest_ratio)

  # Calculate y position for callout (slightly above the point)
  y_range <- diff(range(price_gp_df$price_gp_ratio, na.rm = TRUE))
  callout_y_position <- latest_ratio + (y_range * 0.1)

  # Create the visualization
  ggplot2::ggplot(price_gp_df, ggplot2::aes(x = date, y = price_gp_ratio)) +
    # Add median reference line
    ggplot2::geom_hline(
      yintercept = median_ratio,
      linetype = "dashed",
      color = "gray60",
      alpha = 0.7
    ) +
    ggplot2::geom_line(color = "black", linewidth = 0.5) +
    ggplot2::geom_point(
      data = price_gp_df[price_gp_df$date == latest_date, ],
      color = "black",
      size = 2
    ) +
    # Add callout text
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = callout_y_position,
      label = callout_text,
      hjust = -0.2,
      vjust = -0.5,
      size = 3
    ) +
    # Add median value label
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = median_ratio,
      label = sprintf("Median: %.1fx", median_ratio),
      hjust = -0.2,
      vjust = -0.5,
      size = 2.5,
      color = "gray40"
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 20, r = 50, b = 10, l = 10)
    ) +
    ggplot2::labs(
      title = paste0(ticker, " - P/GP"),
      x = "",
      y = "Price/Gross Profit"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0.05, 0.2))
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.2))
    )
}
