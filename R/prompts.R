#' LLM Prompt Templates
#'
#' Collection of prompt templates for different analysis types
#'
#' @noRd

PROMPTS <- list(
  enhanced_description = paste(
    "Based on this company description, provide a detailed, factual overview covering:",
    "1. Core Products & Services",
    "- What specific products/services does the company provide?",
    "- What are the key features and capabilities of each?",
    "- How is the product/service portfolio structured?\n",
    "2. Customer Base & End Markets",
    "- Who are the primary customers and end users?",
    "- What industries/sectors do they serve?",
    "- What are the typical use cases?\n",
    "3. Value Chain Position",
    "- Where do their products/services fit in customer workflows?",
    "- What problems do they solve?",
    "- How do customers implement/use their solutions?\n",
    "4. Business Model",
    "- How do they generate revenue?",
    "- What is their distribution model?",
    "- What is their customer engagement model?\n",
    "5. Key Comps",
    "- List 3-5 comparable peers with detailed differentiation",
    "Please provide only factual information without analysis or opinions.",
    "If no description was provided, simply say 'Description Not Provided'",
    sep = "\n"
  ),

  shortened_description = paste(
    "Distill the business description into a single concise paragraph.",
    "Start with sector/industry context.",
    "Be specific about products/services and their use cases.",
    "After the paragraph, add a 'tldr;' summary.",
    "Conclude with 'Key Comps:' section listing 1-5 peers.",
    "Format all company names in bold.",
    "If no description was provided, simply say 'Description Not Provided'",
    sep = "\n"
  ),

  systems_analysis = paste(
    "Analyze each business line as a separate system covering:",
    "1. First Principles Decomposition",
    "2. System Components and Relationships",
    "3. Complete System Mapping",
    "4. System Dynamics Analysis",
    "5. Structural Evolution",
    "6. System Vulnerabilities and Opportunities",
    "Conclude with system interactions analysis.",
    sep = "\n"
  ),

  synthesized_analysis = paste(
    "For each business segment, analyze:",
    "1. Current Economic Profile",
    "2. Economic Evolution",
    "3. Strategic Alignment",
    "4. Economic Trajectories",
    "5. High-Confidence Predictions",
    "6. Synthesis and Trajectory",
    "Connect all economic predictions to system dynamics.",
    sep = "\n"
  ),

  upside_case = paste(
    "Create a detailed upside case focusing on:",
    "- Compounding strengths",
    "- Porter's Five Forces advantages",
    "- Positive feedback loops",
    "- Secular trend positioning",
    "- Natural defensive characteristics",
    "Format as a single paragraph with markdown.",
    sep = "\n"
  ),

  downside_case = paste(
    "Create a detailed downside case focusing on:",
    "- Business model vulnerabilities",
    "- Economic limitations",
    "- Competitive pressures",
    "- Negative feedback loops",
    "- Structural challenges",
    "Format as a single paragraph with markdown.",
    sep = "\n"
  ),

  thesis = paste(
    "Synthesize a comprehensive investment thesis covering:",
    "1. Investment Overview (3 sentences)",
    "2. Systems Analysis (4-6 sentences)",
    "3. Risk Assessment (3 sentences)",
    "4. Conclusion (4 sentences)",
    "End with a tldr summary of the long/short thesis.",
    sep = "\n"
  ),

  tldr = "Provide a clear and concise TLDR summary in 2-3 sentences."
)
