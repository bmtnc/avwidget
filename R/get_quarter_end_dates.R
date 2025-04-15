get_quarter_end_dates <- function(start_date, end_date) {
  if (!inherits(start_date, "Date") || !inherits(end_date, "Date")) {
    stop("Both start_date and end_date must be Date objects")
  }
  
  if (start_date > end_date) {
    stop("start_date must be before or equal to end_date")
  }
  
  # Get sequence of quarter starts, including the quarter containing end_date
  quarter_starts <- seq(
    from = lubridate::floor_date(start_date, unit = "quarter"),
    to = lubridate::floor_date(end_date, unit = "quarter"),
    by = "quarter"
  )
  
  # Get all quarter ends, including the one for end_date's quarter
  quarter_ends <- lubridate::ceiling_date(quarter_starts, unit = "quarter") - lubridate::days(1)
  
  # Ensure we include the quarter end for end_date's quarter, even if end_date is before quarter end
  last_quarter_end <- lubridate::ceiling_date(end_date, unit = "quarter") - lubridate::days(1)
  if (last_quarter_end > utils::tail(quarter_ends, 1)) {
    quarter_ends <- c(quarter_ends, last_quarter_end)
  }
  
  quarter_ends
}