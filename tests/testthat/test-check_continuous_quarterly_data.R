test_that("check_continuous_quarterly_data validates continuous quarterly data", {
  # Create sample continuous data
  continuous_data <- tibble::tibble(
    date = as.Date(c("2023-12-31", "2023-09-30", "2023-06-30", "2023-03-31")),
    gross_profit = c(1000, 900, 800, 700),
    ebit = c(500, 450, 400, 350)
  )

  expect_true(check_continuous_quarterly_data(continuous_data))
})

test_that("check_continuous_quarterly_data catches missing quarters", {
  # Create sample data with gap
  data_with_gap <- tibble::tibble(
    date = as.Date(c("2023-12-31", "2023-09-30", "2023-03-31")),
    gross_profit = c(1000, 900, 700),
    ebit = c(500, 450, 350)
  )

  expect_error(
    check_continuous_quarterly_data(data_with_gap),
    "Missing quarterly data for the following dates: 2023-06-30"
  )
})

test_that("check_continuous_quarterly_data works with single quarter", {
  single_quarter <- tibble::tibble(
    date = as.Date("2023-12-31"),
    gross_profit = 1000,
    ebit = 500
  )

  expect_true(check_continuous_quarterly_data(single_quarter))
})

test_that("check_continuous_quarterly_data handles invalid inputs", {
  # Test with wrong data type
  expect_error(
    check_continuous_quarterly_data(list()),
    "Input must be a data.frame or tibble"
  )

  # Test with missing date column
  invalid_data <- tibble::tibble(
    gross_profit = c(1000, 900),
    ebit = c(500, 450)
  )

  expect_error(
    check_continuous_quarterly_data(invalid_data),
    "Financial data must contain a 'date' column"
  )

  # Test with wrong date type
  wrong_date_type <- tibble::tibble(
    date = c("2023-12-31", "2023-09-30"),
    gross_profit = c(1000, 900),
    ebit = c(500, 450)
  )

  expect_error(
    check_continuous_quarterly_data(wrong_date_type),
    "The date column must be of class Date"
  )
})
