#' Calculate Shareholder Ownership Index
#'
#' Calculates an index representing the change in proportional ownership due to
#' share count changes relative to a start date. The index is calculated as
#' 100 * (Shares at Start Date / Shares at Current Date).
#' An index > 100 indicates anti-dilution (buybacks) relative to the start.
#' An index < 100 indicates dilution relative to the start.
#'
#' @param financial_ts A data frame containing financial time series data,
#'   requiring 'date' and 'shares_outstanding' columns. Shares outstanding
#'   are expected to represent the actual number of shares for that period end.
#' @param start_date The date from which to start the calculation (inclusive).
#'   The shares outstanding on the first available date *on or after* this
#'   start_date will be used as the baseline.
#'
#' @return A tibble with 'date' and 'ownership_index'. Returns an empty tibble
#'   if input is invalid or no data is available after start_date.
#' @export
#' @importFrom dplyr %>% filter arrange slice mutate select
#' @importFrom rlang .data
#' @importFrom lubridate is.Date as_date
#' @importFrom tibble tibble
calculate_shareholder_ownership_index <- function(financial_ts, start_date) {

  # --- Input Validation ---
  if (!is.data.frame(financial_ts)) {
    warning("Input 'financial_ts' must be a data frame.")
    return(tibble::tibble(date = as.Date(character()), ownership_index = numeric()))
  }
  if (!all(c("date", "shares_outstanding") %in% names(financial_ts))) {
    warning("Input 'financial_ts' must contain 'date' and 'shares_outstanding' columns.")
     return(tibble::tibble(date = as.Date(character()), ownership_index = numeric()))
  }
   tryCatch({
    start_date <- lubridate::as_date(start_date)
  }, error = function(e) {
    stop("Invalid 'start_date'. Please provide a valid date string or Date object.")
  })
   if (!lubridate::is.Date(financial_ts$date)) {
    tryCatch({
      financial_ts <- financial_ts %>% dplyr::mutate(date = lubridate::as_date(.data$date))
    }, error = function(e) {
      stop("Failed to convert financial_ts$date to Date type.")
    })
  }

  # --- Calculation ---
  data_filtered <- financial_ts %>%
    dplyr::select(date, shares_outstanding) %>%
    dplyr::filter(.data$date >= start_date, !is.na(.data$shares_outstanding), .data$shares_outstanding > 0) %>%
    dplyr::arrange(.data$date)

  if (nrow(data_filtered) == 0) {
    warning("No valid share data found on or after the specified start_date.")
    return(tibble::tibble(date = as.Date(character()), ownership_index = numeric()))
  }

  # Get the shares outstanding at the very start of the filtered period
  shares_start <- data_filtered %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$shares_outstanding)

  if (is.na(shares_start) || shares_start <= 0) {
     warning("Invalid starting shares value found.")
     return(tibble::tibble(date = as.Date(character()), ownership_index = numeric()))
  }

  # Calculate the index
  result_data <- data_filtered %>%
    dplyr::mutate(
      ownership_index = 100 * shares_start / .data$shares_outstanding
    ) %>%
    dplyr::select(date, ownership_index)

  return(result_data)
}