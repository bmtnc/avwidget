#' Get Earnings Call Transcript as Plain Text with Enhanced Fallback
#'
#' @param ticker Character string of the stock ticker symbol
#' @param quarter Character string in format YYYYQN (e.g., "2024Q1")
#' @param api_key Character string containing the Alpha Vantage API key
#' @param max_attempts Integer number of quarters to try (default 4)
#' @return A list containing the transcript text and the quarter it came from
#'
fetch_transcript_text <- function(ticker, quarter, api_key, max_attempts = 4) {
  # Helper function to get previous quarter
  get_previous_quarter <- function(quarter) {
    year <- as.numeric(substr(quarter, 1, 4))
    q <- as.numeric(substr(quarter, 6, 6))
    
    prev_q <- if(q == 1) 4 else q - 1
    prev_year <- if(q == 1) year - 1 else year
    
    sprintf("%dQ%d", prev_year, prev_q)
  }
  
  # Helper function to try getting transcript for a specific quarter
  try_get_transcript <- function(q) {
    tryCatch({
      response <- make_alpha_vantage_request(
        function_name = "EARNINGS_CALL_TRANSCRIPT",
        ticker = ticker,
        api_key = api_key,
        quarter = q
      )
      
      if (!is.null(response$transcript) && length(response$transcript) > 0) {
        transcript_text <- paste(
          sapply(response$transcript, function(x) x$content),
          collapse = " "
        )
        
        if (nchar(transcript_text) > 100) { # Basic validation
          return(list(
            text = transcript_text,
            quarter = q
          ))
        }
      }
      return(NULL)
    }, error = function(e) {
      message(sprintf("Error fetching %s transcript for %s: %s", q, ticker, e$message))
      return(NULL)
    })
  }
  
  # Try multiple quarters
  current_quarter <- quarter
  attempts <- 0
  
  while (attempts < max_attempts) {
    message(sprintf("Attempting to fetch transcript for %s - %s", ticker, current_quarter))
    
    result <- try_get_transcript(current_quarter)
    if (!is.null(result)) {
      if (current_quarter != quarter) {
        message(sprintf("Using transcript from %s instead of requested %s", 
                       current_quarter, quarter))
      }
      return(result)
    }
    
    current_quarter <- get_previous_quarter(current_quarter)
    attempts <- attempts + 1
    
    # Rate limiting
    Sys.sleep(1)
  }
  
  stop(sprintf("No transcript available for %s in last %d quarters", ticker, max_attempts))
}