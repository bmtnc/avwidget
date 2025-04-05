#' Get Enhanced Company Description
#'
#' @param description Original company description
#' @param ticker Company ticker symbol
#' @return Enhanced description
#' @export
get_enhanced_description <- function(description, ticker) {
  llm_request(
    prompt = PROMPTS$enhanced_description,
    context = description,
    cache_key = ticker,
    response_type = "enhanced_description",
    max_tokens = 1000
  )
}

#' Get Shortened Description
#'
#' @param enhanced_description Enhanced company description
#' @param ticker Company ticker symbol
#' @return Shortened description with TLDR and comps
#' @export
get_shortened_description <- function(enhanced_description, ticker) {
  llm_request(
    prompt = PROMPTS$shortened_description,
    context = enhanced_description,
    cache_key = ticker,
    response_type = "shortened_description",
    max_tokens = 500
  )
}

#' Get Systems Analysis
#'
#' @param enhanced_description Enhanced company description
#' @param ticker Company ticker symbol
#' @return Detailed systems analysis
#' @export
get_systems_analysis <- function(enhanced_description, ticker) {
  llm_request(
    prompt = PROMPTS$systems_analysis,
    context = enhanced_description,
    cache_key = ticker,
    response_type = "systems_analysis",
    max_tokens = 2000,
    model = "claude-3-5-sonnet-latest"  # Using larger model for complex analysis
  )
}

#' Get Synthesized Analysis
#'
#' @param systems_analysis Systems analysis text
#' @param ticker Company ticker symbol
#' @return Economic analysis and predictions
#' @export
get_synthesized_analysis <- function(systems_analysis, ticker) {
  llm_request(
    prompt = PROMPTS$synthesized_analysis,
    context = systems_analysis,
    cache_key = ticker,
    response_type = "synthesized_analysis",
    max_tokens = 1500,
    model = "claude-3-5-sonnet-latest"
  )
}

#' Get Upside Case
#'
#' @param detailed_analysis Synthesized analysis text
#' @param ticker Company ticker symbol
#' @return Upside case analysis
#' @export
get_upside_case <- function(detailed_analysis, ticker) {
  llm_request(
    prompt = PROMPTS$upside_case,
    context = detailed_analysis,
    cache_key = ticker,
    response_type = "upside_case",
    max_tokens = 400
  )
}

#' Get Downside Case
#'
#' @param detailed_analysis Synthesized analysis text
#' @param ticker Company ticker symbol
#' @return Downside case analysis
#' @export
get_downside_case <- function(detailed_analysis, ticker) {
  llm_request(
    prompt = PROMPTS$downside_case,
    context = detailed_analysis,
    cache_key = ticker,
    response_type = "downside_case",
    max_tokens = 400
  )
}

#' Get Investment Thesis
#'
#' @param sys_analysis Systems analysis text
#' @param synthesized_analysis Synthesized analysis text
#' @param upside_case Upside case text
#' @param downside_case Downside case text
#' @param ticker Company ticker symbol
#' @return Comprehensive investment thesis
#' @export
get_thesis <- function(sys_analysis, synthesized_analysis, upside_case, downside_case, ticker) {
  context <- sprintf(
    "SYSTEMS ANALYSIS:\n%s\n\nDETAILED ANALYSIS:\n%s\n\nUPSIDE CASE:\n%s\n\nDOWNSIDE CASE:\n%s",
    sys_analysis, synthesized_analysis, upside_case, downside_case
  )
  
  llm_request(
    prompt = PROMPTS$thesis,
    context = context,
    cache_key = ticker,
    response_type = "investment_thesis",
    max_tokens = 1000,
    model = "claude-3-5-sonnet-latest"
  )
}

#' Get TLDR Summary
#'
#' @param input_text Text to summarize
#' @param ticker Company ticker symbol (optional, for caching)
#' @return Concise TLDR summary
#' @export
get_tldr <- function(input_text, ticker = NULL) {
  llm_request(
    prompt = PROMPTS$tldr,
    context = input_text,
    cache_key = ticker,
    response_type = "tldr",
    max_tokens = 200
  )
}