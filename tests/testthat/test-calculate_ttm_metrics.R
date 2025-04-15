library(testthat)
library(tibble)

test_that("calculate_ttm_metrics matches expected output", {
  # Input data
  input_data <- tibble::tribble(
    ~date,           ~revenue, ~gross_profit, ~net_income,
    "2022-01-01",    100,      50,           20,
    "2022-04-01",    120,      60,           25,
    "2022-07-01",    130,      65,           27,
    "2022-10-01",    150,      75,           30,
    "2023-01-01",    160,      80,           32,
    "2023-04-01",    170,      85,           35,
    "2023-07-01",    180,      90,           37,
    "2023-10-01",    190,      95,           40
  ) %>%
    dplyr::mutate(date = as.Date(date))

  # Expected output - including original columns plus TTM
  expected_output <- tibble::tribble(
    ~date,           ~revenue, ~gross_profit, ~net_income, ~revenue_ttm, ~gross_profit_ttm, ~net_income_ttm,
    "2022-01-01",    100,      50,           20,          NA,           NA,                NA,
    "2022-04-01",    120,      60,           25,          NA,           NA,                NA,
    "2022-07-01",    130,      65,           27,          NA,           NA,                NA,
    "2022-10-01",    150,      75,           30,          500,          250,               102,
    "2023-01-01",    160,      80,           32,          560,          280,               114,
    "2023-04-01",    170,      85,           35,          610,          305,               124,
    "2023-07-01",    180,      90,           37,          660,          330,               134,
    "2023-10-01",    190,      95,           40,          700,          350,               144
  ) %>%
    dplyr::mutate(date = as.Date(date))

  # Run calculation
  actual_output <- calculate_ttm_metrics(input_data)

  # Test that actual matches expected
  expect_equal(actual_output, expected_output)
})
