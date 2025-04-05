#' Clear LLM response cache
#'
#' @param ticker Optional character string to clear cache for specific ticker
#' @param cache_dir Character string of cache directory path
#'
#' @return Invisible NULL
#' @export
clear_llm_cache <- function(ticker = NULL, cache_dir = "llm_cache") {
  if (!dir.exists(cache_dir)) return(invisible(NULL))

  if (is.null(ticker)) {
    # Clear all cache
    unlink(cache_dir, recursive = TRUE)
    dir.create(cache_dir)
  } else {
    # Clear cache for specific ticker
    files <- list.files(
      cache_dir,
      pattern = paste0("^", ticker, "_"),
      full.names = TRUE
    )
    unlink(files)
  }

  invisible(NULL)
}
