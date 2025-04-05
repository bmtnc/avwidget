#' Analyze Earnings Call Fundamentals
#'
#' @param transcript_result List containing $text and $quarter from transcript fetch
#' @param ticker Company ticker symbol
#' @return Analysis of fundamental results and developments
#' @export
analyze_earnings_fundamentals <- function(transcript_result, ticker) {
  prompt <- paste0(
    sprintf("Analyze this earnings call transcript from %s focusing ONLY on fundamental business results, developments, and forward-looking trends.", 
            transcript_result$quarter),
    "\n\nKey areas to address:",
    "\n- Revenue and margin trends",
    "\n- End market demand signals",
    "\n- Technical or product developments",
    "\n- Geographic or segment performance",
    "\n- Supply chain or operational updates",
    "\n\nImportant context:",
    "\n- Focus on quantifiable metrics and specific details",
    "\n- Strip away management's fluffy commentary and corporate jargon",
    "\n- Distinguish between actual results and forward-looking statements",
    "\n- Note any inconsistencies between metrics and management's narrative",
    "\n\nProvide a concise, objective analysis focusing on material information only."
  )

  llm_request(
    prompt = prompt,
    context = transcript_result$text,
    cache_key = sprintf("%s_%s_fundamentals", ticker, transcript_result$quarter),
    response_type = "earnings_fundamentals",
    max_tokens = 1000,
    model = "claude-3-5-sonnet-latest"
  )
}

#' Analyze Earnings Call Q&A
#'
#' @param transcript_result List containing $text and $quarter from transcript fetch
#' @param ticker Company ticker symbol
#' @return Analysis of analyst questions and management responses
#' @export
analyze_earnings_qa <- function(transcript_result, ticker) {
  prompt <- paste0(
    sprintf("Analyze the Q&A section of this earnings call transcript from %s, focusing on sell-side analyst questions and management responses.",
            transcript_result$quarter),
    "\n\nKey areas to examine:",
    "\n- What specific concerns or doubts are analysts raising through their questions?",
    "\n- Which topics received multiple questions or follow-ups?",
    "\n- Were management's responses direct and specific, or vague and dismissive?",
    "\n- What themes emerge from the line of questioning?",
    "\n- Which analysts seemed skeptical vs. optimistic?",
    "\n\nImportant context:",
    "\n- The strongest signal comes from analysts' line of questioning",
    "\n- Management is trained to spin positive narratives",
    "\n- Look for disconnects between management responses and analyst concerns",
    "\n- Note any topics management seemed to avoid or deflect",
    "\n\nProvide an analysis that:",
    "\n1. Identifies key themes in analyst questioning",
    "\n2. Evaluates quality and directness of management responses",
    "\n3. Highlights any notable skepticism or pressing concerns",
    "\n4. Notes any disconnect between management's narrative and analysts' focus"
  )

  llm_request(
    prompt = prompt,
    context = transcript_result$text,
    cache_key = sprintf("%s_%s_qa_analysis", ticker, transcript_result$quarter),
    response_type = "earnings_qa",
    max_tokens = 1500,
    model = "claude-3-5-sonnet-latest"
  )
}

#' Get Complete Earnings Call Analysis
#'
#' @param transcript_result List containing $text and $quarter from transcript fetch
#' @param ticker Character string of the stock ticker symbol
#' @return List containing both fundamental and Q&A analysis plus the quarter analyzed
#' @export
analyze_earnings_call <- function(transcript_result, ticker) {
  if (!all(c("text", "quarter") %in% names(transcript_result))) {
    stop("transcript_result must contain both 'text' and 'quarter' elements")
  }
  
  # Perform both analyses
  fundamentals_analysis <- analyze_earnings_fundamentals(
    transcript_result = transcript_result,
    ticker = ticker
  )
  
  qa_analysis <- analyze_earnings_qa(
    transcript_result = transcript_result,
    ticker = ticker
  )

  # Return results including which quarter was analyzed
  list(
    fundamentals = fundamentals_analysis,
    qa = qa_analysis,
    quarter = transcript_result$quarter
  )
}

#' Generate Business Narrative from Earnings Call Transcript
#'
#' @param transcript_result List containing $text and $quarter from transcript fetch
#' @param ticker Character string of the stock ticker symbol
#' @return Character string containing the business narrative
#' @importFrom magrittr %>%
#' @export
generate_business_narrative <- function(transcript_result, ticker) {
  if (!all(c("text", "quarter") %in% names(transcript_result))) {
    stop("transcript_result must contain both 'text' and 'quarter' elements")
  }

  # Craft a focused prompt for business analysis
  prompt <- sprintf(
    "You are a seasoned buy-side analyst known for your ability to distill complex businesses into clear, insightful narratives. Read this earnings call transcript for %s (from %s) and create a single paragraph that addresses:

    1. What does this business fundamentally do?
    2. What is the long-term trajectory of the business?
    3. What are the key factors that make this business either:
       - Exciting (strong competitive advantages, growth runway)
       - Concerning (competitive threats, structural challenges)
       - Unimpressive (lack of differentiation, poor economics)
    
    Focus on being objective and analytical. Avoid management's promotional language. Look for concrete evidence in the transcript that supports your conclusions.",
    ticker, transcript_result$quarter
  )

  llm_request(
    prompt = prompt,
    context = transcript_result$text,
    model = "claude-3-5-haiku-latest",
    max_tokens = 500,
    temperature = 0,
    cache_key = sprintf("%s_%s_narrative", ticker, transcript_result$quarter),
    response_type = "business_narrative",
    cache_dir = "cache/business_narratives"
  )
}