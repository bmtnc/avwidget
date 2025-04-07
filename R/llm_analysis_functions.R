#' Generate Secular Trends Analysis
#'
#' @param enhanced_description Enhanced business description
#' @param ticker Company ticker symbol
#' @return Brief analysis of relevant secular trends
#' @export
analyze_secular_trends <- function(enhanced_description, ticker) {
  prompt <- paste0(
    "As a strategic analyst, analyze how this business relates to major secular trends. ",
    "Consider both obvious trends mentioned in the description AND important trends that may not be directly mentioned. ",
    "\n\nProvide a brief (2-3 sentences) analysis that must include:",
    "\n1. 1-2 secular trends the company is well-aligned with (if any exist), explaining the alignment",
    "\n2. 1 secular trend that could pose challenges or where the company may be misaligned",
    "\n\nImportant guidelines:",
    "\n- Not every company benefits from major secular trends - avoid forcing connections",
    "\n- Consider geopolitical shifts, technological evolution, demographic changes, regulatory trends",
    "\n- Be specific about why/how the trend matters to this business",
    "\n- Focus on structural, long-term shifts rather than cyclical patterns",
    "\n- Keep the language concise and analytical",
    "\n\nFormat the response as 'Secular Trends:' followed by your analysis in a single paragraph."
  )

  llm_request(
    prompt = prompt,
    context = enhanced_description,
    cache_key = sprintf("%s_secular_trends", ticker),
    response_type = "secular_trends",
    max_tokens = 200,
    model = "claude-3-5-haiku-latest"
  )
}