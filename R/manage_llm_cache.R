#' Manage LLM response caching
#'
#' @param ticker Character string of the stock ticker
#' @param response_type Character string indicating type of response (e.g., "enhanced_description")
#' @param generate_fn Function to generate new response if needed
#' @param cache_dir Character string of directory path for cache files
#'
#' @return Character string of LLM response
#' @export
manage_llm_cache <- function(ticker, response_type, generate_fn, cache_dir = "llm_cache") {
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Create filename
  filename <- file.path(cache_dir, paste0(ticker, "_", response_type, ".txt"))

  # Check if file exists
  if (file.exists(filename)) {
    # Read existing response
    response <- readLines(filename, warn = FALSE) |>
      paste(collapse = "\n")
  } else {
    # Generate new response
    response <- generate_fn()

    # Save response only if it's not an error message
    if (!identical(response, "LLM currently unavailable.")) {
      writeLines(response, filename)
    }
  }

  return(response)
}
