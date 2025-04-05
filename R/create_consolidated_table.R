#' Create and Display Formatted GT Table for Financial Metrics
#'
#' Takes a metrics dataframe and creates a formatted GT table
#'
#' @param metrics_df A tibble containing financial metrics
#' @return A GT table object
#'
#' @importFrom gt gt tab_style tab_options
create_consolidated_table <- function(metrics_df) {
  if (!all(c("Metric1", "Value1", "Metric2", "Value2", "Metric3", "Value3") %in%
           names(metrics_df))) {
    stop(paste0("Missing required columns in metrics_df"))
  }

  gt::gt(metrics_df) %>%
    gt::cols_align(align = "left", columns = c("Metric1", "Metric2", "Metric3")) %>%
    gt::cols_align(align = "right", columns = c("Value1", "Value2", "Value3")) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.width = gt::pct(100),
      table.border.top.style = "solid",
      table.border.bottom.style = "solid",
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      table.font.size = "11px",
      data_row.padding = gt::px(2),  # Corrected option name
      row_group.border.top.style = "solid",
      row_group.border.bottom.style = "solid",
      heading.title.font.size = "0px",
      heading.padding = gt::px(0)
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "gray90",
          weight = gt::px(1)
        )
      ),
      locations = list(
        gt::cells_body()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "gray97"),
      locations = gt::cells_body(rows = seq(1, nrow(metrics_df), 2))
    )
}
