#' Analyze Earnings Call Fundamentals
#'
#' @param transcript_result List containing $text and $quarter from transcript fetch
#' @param ticker Company ticker symbol
#' @return Analysis of fundamental results and developments
#' @export
analyze_earnings_fundamentals <- function(transcript_result, ticker) {
  prompt <- paste0(
    sprintf("Analyze this earnings call transcript from %s. Present your analysis in R markdown format with a brief narrative overview followed by organized sections with bullet points.", 
            transcript_result$quarter),
    "\n\nStructure:",
    "\nStart with 2-3 sentences summarizing the key narrative and most important takeaways.",
    "\n\nThen organize the details into these sections (use ### for headers):",
    "\n### Financial Performance",
    "\n- Revenue and margins",
    "\n- Segment results",
    "\n- Geographic performance",
    "\n### Market & Operations",
    "\n- End market trends",
    "\n- Product/technical developments",
    "\n- Supply chain/operations",
    "\n### Forward Outlook",
    "\n- Guidance",
    "\n- Strategic initiatives",
    "\n- Management commentary on trends",
    "\n\nImportant guidelines:",
    "\n- Use bullet points for specific metrics and details",
    "\n- Focus on quantifiable results and concrete statements",
    "\n- Strip away promotional language",
    "\n- Note any inconsistencies between metrics and narrative",
    "\n- Keep bullets concise but preserve important nuance",
    "\n- Maintain proper markdown list formatting with blank lines between sections",
    "\n- Use * for bullet points with 4 spaces for nesting",
    "\n- Ensure each bullet point starts on a new line"
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
    sprintf("Analyze the Q&A section of this earnings call transcript from %s. Present your analysis in R markdown format with a narrative summary followed by structured insights.", 
            transcript_result$quarter),
    "\n\nStructure:",
    "\nStart with 2-3 sentences capturing the key dynamic between analysts and management.",
    "\n\nThen organize insights into these sections (use ### for headers):",
    "\n### Key Themes & Concerns",
    "\n- Major topics that received multiple questions",
    "\n- Specific concerns raised by analysts",
    "\n### Management Response Quality",
    "\n- Areas where responses were detailed and direct",
    "\n- Topics where management seemed evasive or vague",
    "\n### Notable Dynamics",
    "\n- Specific instances of analyst skepticism",
    "\n- Key disconnects between management and analyst perspectives",
    "\n\nImportant guidelines:",
    "\n- Use bullet points for specific examples and details",
    "\n- Focus on actual dialogue, not management spin",
    "\n- Preserve important nuance while being concise",
    "\n- Note analyst names when highlighting significant exchanges",
    "\n- Look for patterns in questioning and response quality",
    "\n- Maintain proper markdown list formatting with blank lines between sections",
    "\n- Use * for bullet points with 4 spaces for nesting",
    "\n- Ensure each bullet point starts on a new line"
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

#' Generate Enhanced Business Description from Multiple Sources
#'
#' @param alpha_vantage_desc Original company description from Alpha Vantage
#' @param transcript_result List containing $text and $quarter from transcript fetch
#' @param ticker Company ticker symbol
#' @return Enhanced 200-word business description
#' @export
generate_combined_description <- function(alpha_vantage_desc, transcript_result, ticker) {
  # Combine context sources
  combined_context <- sprintf(
    "ALPHA VANTAGE DESCRIPTION:\n%s\n\nLATEST EARNINGS CALL (%s):\n%s",
    alpha_vantage_desc,
    transcript_result$quarter,
    transcript_result$text
  )
  
  prompt <- paste0(
    "You are a highly skilled equity analyst. Based on the provided company description and earnings call transcript, ",
    "write a comprehensive yet concise (200 words, single paragraph) business description that addresses:\n",
    "\n1. Core business model and how the company makes money",
    "\n2. Key competitive advantages or differentiation",
    "\n3. Market position and growth drivers",
    "\n4. Critical business metrics or trends",
    "\n5. Major risks or challenges",
    "\n\nFocus on concrete facts and numbers from the earnings call while avoiding promotional language. ",
    "The description should give an investor a clear understanding of what drives this business."
  )

  llm_request(
    prompt = prompt,
    context = combined_context,
    cache_key = sprintf("%s_combined_description", ticker),
    response_type = "combined_description",
    max_tokens = 300
  )
}