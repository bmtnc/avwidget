#' Generate financial dashboard report
#'
#' @param ticker Character string of the stock ticker symbol
#' @param latest_price Numeric value of the latest known price (optional)
#' @param latest_price_date Date of the latest price (defaults to current date if latest_price is provided)
#' @return Vector of paths to generated reports
#'
generate_report <- function(ticker, 
                          latest_price = NULL, 
                          latest_price_date = Sys.Date()) {
  
  # Validate inputs if latest price is provided
  if (!is.null(latest_price)) {
    if (!is.numeric(latest_price) || latest_price <= 0) {
      stop("latest_price must be a positive number")
    }
    if (!inherits(latest_price_date, "Date")) {
      latest_price_date <- as.Date(latest_price_date)
    }
  }
  
  # Generate report
  base_name <- paste0(ticker, "_", Sys.Date())
  output_dir <- "reports"
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Render both formats
  output_files <- c()

  # HTML output
  html_file <- rmarkdown::render(
    input = "dashboard.Rmd",
    output_format = "html_document",
    output_file = paste0(base_name, ".html"),
    output_dir = output_dir,
    params = list(
      ticker = ticker,
      latest_price = latest_price,
      latest_price_date = latest_price_date
    ),
    envir = new.env()
  )
  output_files <- c(output_files, html_file)
  
  # Markdown output
  md_file <- rmarkdown::render(
    input = "dashboard.Rmd",
    output_format = "github_document",
    output_file = paste0(base_name, ".md"),
    output_dir = output_dir,
    params = list(
      ticker = ticker,
      latest_price = latest_price,
      latest_price_date = latest_price_date
    ),
    envir = new.env()
  )
  output_files <- c(output_files, md_file)

  return(output_files)
}