#' Make a request to an LLM API
#'
#' Core function for making LLM API requests with standardized parameters and error handling
#'
#' @param prompt Character string containing the prompt
#' @param context Character string containing relevant context (optional)
#' @param model Character string specifying model to use 
#' @param api_key Character string containing API key
#' @param max_tokens Integer specifying maximum tokens in response
#' @param temperature Numeric value controlling randomness (0-1)
#' @param cache_key Character string for caching (optional)
#' @param response_type Character string specifying type of response for cache naming
#' @param cache_dir Character string specifying cache directory (optional)
#' @return Character string containing the LLM response
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom magrittr %>%
llm_request <- function(
  prompt,
  context = NULL,
  model = "claude-3-5-haiku-latest",
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  max_tokens = 1000,
  temperature = 0,
  cache_key = NULL,
  response_type = "default",
  cache_dir = "llm_cache"
) {
# Input validation
if (!is.character(prompt) || length(prompt) != 1) {
  stop("prompt must be a single character string")
}

valid_models <- c("claude-3-5-sonnet-latest", "claude-3-5-haiku-latest")
if (!model %in% valid_models) {
  stop(sprintf("Invalid model. Must be one of: %s", paste(valid_models, collapse = ", ")))
}

if (nchar(api_key) < 10) {
  stop("Invalid API key")
}

# Combine prompt with context if provided
full_prompt <- if (!is.null(context)) {
  sprintf("Context:\n%s\n\nPrompt:\n%s", context, prompt)
} else {
  prompt
}

# Define generator function for cache
generate_fn <- function() {
  body <- list(
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    messages = list(
      list(
        role = "user",
        content = full_prompt
      )
    )
  )

  tryCatch({
    response <- httr2::request("https://api.anthropic.com/v1/messages") %>%
      httr2::req_headers(
        "x-api-key" = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type" = "application/json"
      ) %>%
      httr2::req_body_json(body) %>%
      httr2::req_perform()

    result <- httr2::resp_body_json(response)
    result$content[[1]]$text

  }, error = function(e) {
    warning(sprintf("LLM API call failed: %s", e$message))
    return("LLM response currently unavailable.")
  })
}

# Use cache if cache_key provided
if (!is.null(cache_key)) {
  manage_llm_cache(cache_key, response_type, generate_fn, cache_dir)
} else {
  generate_fn()
}
}