#' Create EBIT Growth Chart
#'
#' Creates a combination chart showing TTM EBIT trend and YoY growth rates
#'
#' @param financial_data DataFrame with quarterly financial data including ebit
#' @param ticker Character string of the stock ticker symbol
#' @param start_date Start date for the chart (default: "2018-01-01")
#' @return A grid arranged plot with TTM EBIT and growth rates
#'
#' @importFrom ggplot2 ggplot aes geom_line theme labs scale_y_continuous scale_x_date
#' @importFrom gridExtra grid.arrange
create_ebit_growth_chart <- function(financial_data, ticker, start_date = "2018-01-01") {
  # Input validation
  if (!all(c("date", "ebit") %in% names(financial_data))) {
    stop("financial_data must contain 'date' and 'ebit' columns")
  }

  # Calculate TTM EBIT and YoY Growth in one pipeline
  plot_data <- financial_data %>%
    dplyr::mutate(ebit = as.numeric(ebit)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      # Calculate TTM using roll
      ttm_ebit = roll::roll_sum(ebit, width = 4, min_obs = 4)
    ) %>%
    # Calculate YoY growth correctly
    dplyr::mutate(
      prior_year_ttm = dplyr::lag(ttm_ebit, 4),
      yoy_growth = case_when(
        is.na(prior_year_ttm) ~ NA_real_,
        prior_year_ttm == 0 ~ NA_real_,
        TRUE ~ ((ttm_ebit - prior_year_ttm) / abs(prior_year_ttm)) * 100
      ),
      growth_color = ifelse(yoy_growth >= 0, "positive", "negative")
    ) %>%
    # Filter for complete cases and date range
    dplyr::filter(
      !is.na(ttm_ebit),
      date >= lubridate::as_date(start_date)
    )

  # Get latest values for annotations
  latest_data <- plot_data %>% dplyr::slice_tail(n = 1)

  # Define shared date scale
  date_scale <- ggplot2::scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  )

  # Create TTM EBIT chart
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = ttm_ebit)) +
    ggplot2::geom_col(fill = "steelblue", width = 85) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0, 0.5), "cm")
    ) +
    ggplot2::labs(
      title = "TTM EBIT"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::dollar_format(scale = 1e-9, suffix = "B"),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    date_scale +
    # Add callout for latest EBIT value
    ggplot2::annotate(
      "text",
      x = latest_data$date,
      y = latest_data$ttm_ebit,
      label = scales::dollar(latest_data$ttm_ebit, scale = 1e-9, suffix = "B"),
      vjust = -0.5,
      size = 3
    )

  # Create YoY Growth chart
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = yoy_growth)) +
    ggplot2::geom_col(
      aes(fill = growth_color),
      width = 85
    ) +
    ggplot2::scale_fill_manual(
      values = c("positive" = "forestgreen", "negative" = "red"),
      guide = "none"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0, 0.5, 0.5, 0.5), "cm")
    ) +
    ggplot2::labs(
      y = "YoY Growth (%)"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(suffix = "%"),
      limits = function(x) {
        max_abs <- max(abs(x), na.rm = TRUE)
        c(-max_abs, max_abs) # Symmetrical limits
      },
      expand = ggplot2::expansion(mult = c(0.1, 0.1))
    ) +
    date_scale +
    # Add callout for latest growth value
    ggplot2::annotate(
      "text",
      x = latest_data$date,
      y = latest_data$yoy_growth,
      label = scales::number(latest_data$yoy_growth, suffix = "%", accuracy = 0.1),
      vjust = ifelse(latest_data$yoy_growth >= 0, -0.5, 1.5),
      size = 3
    )

  # Combine charts using gridExtra
  gridExtra::grid.arrange(
    p1, p2,
    heights = c(2, 1),
    ncol = 1
  )
}
