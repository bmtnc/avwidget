#' Plot Cumulative Change in Shareholder Proportional Interest
#'
#' Creates a line chart illustrating the cumulative impact of share count
#' changes on a shareholder's proportional ownership interest relative to a
#' start date. A smoothed trend line is shown.
#'
#' @param df A data frame containing the ownership index data, requiring
#'   'date' and 'ownership_index' columns (output from
#'   `calculate_shareholder_ownership_index`).
#' @param ticker A character string representing the stock ticker symbol.
#' @param start_date Optional start date (YYYY-MM-DD string or Date object)
#'   to filter the data for the chart. If NULL, uses all data in df.
#'
#' @return A ggplot2 object containing the chart.
#' @export
#' @import ggplot2
#' @importFrom dplyr %>% filter arrange slice pull mutate last
#' @importFrom rlang .data
#' @importFrom scales number
#' @importFrom lubridate as_date is.Date
#' @importFrom stringr str_wrap
#' @importFrom stats loess predict
plot_ownership_index <- function(df, ticker, start_date = NULL) {

  # --- Input Validation ---
  if (!is.data.frame(df)) {
    stop(paste0("Expected df to be a data frame, but received: ", class(df)))
  }
  required_cols <- c("date", "ownership_index")
  if (!all(required_cols %in% names(df))) {
    stop(paste0("df must contain columns: ", paste(required_cols, collapse = ", "),
                ". Found columns: ", paste(names(df), collapse = ", ")))
  }
   if (!lubridate::is.Date(df$date)) {
    tryCatch({
      df <- df %>% dplyr::mutate(date = lubridate::as_date(.data$date))
    }, error = function(e) {
      stop("Failed to convert df$date to Date type.")
    })
  }

  # --- Data Filtering ---
  df_filtered <- df
  if (!is.null(start_date)) {
    tryCatch({
      start_date_obj <- lubridate::as_date(start_date)

      dates_to_consider <- df$date[df$date >= start_date_obj]
      if(length(dates_to_consider) == 0) {
          stop("No dates found on or after the specified start_date.")
      }
      first_valid_date <- min(dates_to_consider, na.rm = TRUE)

      if (!is.finite(first_valid_date)) {
          stop("Could not determine a valid finite start date from the data after filtering.")
      }
      df_filtered <- df %>%
        dplyr::filter(.data$date >= first_valid_date)
    }, error = function(e) {
      warning("Invalid start_date provided or processing error. Using all available data. Error: ", e$message)
      df_filtered <- df
    })
  }

  if (nrow(df_filtered) < 2) {
    stop("Not enough data available for the specified period to draw a line.")
  }

  # --- Prepare for Plotting ---
  df_filtered <- df_filtered %>%
    dplyr::arrange(.data$date)

  # Original dataset (before anchor point) for reference
  df_orig <- df_filtered

  # Save raw values
  latest_date <- dplyr::last(df_orig$date)
  latest_index_raw <- dplyr::last(df_orig$ownership_index)

  # Define color
  line_color <- "#006400" # Dark Green

  # Need at least a few points for smoothing
  min_points_for_smooth <- 4
  can_smooth <- nrow(df_filtered) >= min_points_for_smooth

  # Initialize dot position
  dot_y_position <- latest_index_raw # Default to raw value

  # --- Add anchor point for smoothing ---
  if (can_smooth) {
    tryCatch({
      # Add an artificial anchor point at 100
      first_date <- min(df_filtered$date)
      anchor_date <- first_date - 1 # One day before
      
      df_filtered <- rbind(
        data.frame(
          date = anchor_date,
          ownership_index = 100
        ),
        df_filtered
      )
    }, error = function(e){
      warning("Failed to add anchor point. Proceeding with original data.")
    })
  }

  # --- Create the plot without endpoint dot yet ---
  p <- ggplot2::ggplot(df_filtered, ggplot2::aes(x = .data$date, y = .data$ownership_index)) +
    # Reference line at 100
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
    
    # Smoothed line OR Regular line
    {
      if (can_smooth) {
        ggplot2::geom_smooth(
          method = "loess",
          span = 0.3,
          se = FALSE,
          color = line_color,
          linewidth = 0.8
        )
      } else {
        ggplot2::geom_line(color = line_color, linewidth = 0.7)
      }
    }
  
  # --- Extract smoothed endpoint value ---
  if (can_smooth) {
    tryCatch({
      # Build the plot data
      build <- ggplot2::ggplot_build(p)
      
      # Extract the smoothed line data
      line_data <- build$data[[2]] # This should be the smoothed line
      
      # Get the last point of the smoothed line
      last_point <- line_data[nrow(line_data), ]
      dot_y_position <- last_point$y
      
      # Validate the extracted value
      if (!is.numeric(dot_y_position) || !is.finite(dot_y_position)) {
        warning("Extracted smoothed endpoint is not valid. Using raw value.")
        dot_y_position <- latest_index_raw
      }
    }, error = function(e) {
      warning("Failed to extract smoothed endpoint. Using raw value. Error: ", e$message)
      dot_y_position <- latest_index_raw
    })
  }

  # --- Add endpoint dot and annotation ---
  endpoint_data <- data.frame(
    date = latest_date,
    index_value = dot_y_position
  )

  # Create annotation text (always using raw value)
  index_annotation <- base::paste0(
    "Current: ", base::format(base::round(latest_index_raw, 1), nsmall = 1)
  )

  # Define caption text and wrap it
  caption_text <- stringr::str_wrap(
    paste("Index = 100 * (Shares Outstanding at Start / Shares Outstanding at Date).",
          "Represents the cumulative percentage change in proportional ownership",
          "resulting from share count changes relative to the start date.",
          "Values > 100 indicate anti-dilution (e.g., buybacks); values < 100 indicate dilution."),
    width = 100
  )

  # --- Complete the plot ---
  p <- p +
    # Add endpoint dot at smoothed position
    ggplot2::geom_point(
      data = endpoint_data,
      ggplot2::aes(x = .data$date, y = .data$index_value),
      color = line_color,
      size = 2.5
    ) +
    # Add annotation 
    ggplot2::annotate(
      "text",
      x = latest_date,
      y = dot_y_position,
      label = index_annotation,
      hjust = -0.1,
      vjust = 0.5,
      size = 2.5,
      color = line_color,
      lineheight = 0.9
    ) +
    # Add styling
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "gray90"),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray30"),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50"),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(t = 20, r = 60, b = 10, l = 10)
    ) +
    ggplot2::labs(
      title = paste(ticker, "Cumulative Change in Shareholders' Economic Interest"),
      subtitle = "Impact of Share Count Changes Since Start Date",
      caption = caption_text,
      x = "",
      y = ""
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.05, 0.2))
    ) +
    ggplot2::scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.15))
    )

  return(p)
}