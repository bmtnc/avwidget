test_that("historical market cap is calculated correctly", {
  # Setup - Common Test Data - with continuous adjusted_close values
  price_data <- tibble::tribble(
    ~date,            ~adjusted_close,
    "2023-01-01",     100,
    "2023-01-08",     102,
    "2023-01-15",     105,
    "2023-01-22",     103,
    "2023-01-29",     106,
    "2023-02-05",     108,
    "2023-02-12",     107,
    "2023-02-19",     106,
    "2023-02-26",     108,
    "2023-03-05",     110,
    "2023-03-12",     112,
    "2023-03-19",     111
  ) %>% dplyr::mutate(date = as.Date(date))

  # Financial data that includes a change in shares outstanding
  financial_ts_with_split <- tibble::tribble(
    ~date,            ~shares_equiv_current_shares,
    "2022-12-31",     1000,
    "2023-02-05",     2100,    # Share count changes (could be due to split, issuance, buyback, etc.)
    "2023-03-31",     2100
  ) %>% dplyr::mutate(date = as.Date(date))

  # Test 1: Basic calculation with changing share count
  result1 <- calculate_historical_market_cap(price_data, financial_ts_with_split)
  expected_market_cap <- c(
    100 * 1000, 102 * 1000, 105 * 1000, 103 * 1000, 106 * 1000,
    108 * 2100, 107 * 2100, 106 * 2100, 108 * 2100, 110 * 2100, 112 * 2100, 111 * 2100
  )
  expect_equal(result1$market_cap, expected_market_cap)

  # Test 2: Check column names and types
  expect_true("date" %in% names(result1))
  expect_true("adjusted_close" %in% names(result1))
  expect_true("shares_equiv_current_shares_ffill" %in% names(result1))
  expect_true("market_cap" %in% names(result1))

  expect_s3_class(result1$date, "Date")
  expect_type(result1$adjusted_close, "double")
  expect_type(result1$shares_equiv_current_shares_ffill, "double")
  expect_type(result1$market_cap, "double")

  # Test 3: Only dates on or after the first financial observation should be kept
  price_data_early <- tibble::tribble(
    ~date,            ~adjusted_close,
    "2022-12-25",     95,
    "2022-12-26",     96,
    "2022-12-27",     97,
    "2022-12-31",     98
  ) %>% dplyr::mutate(date = as.Date(date))

  financial_ts_later <- tibble::tribble(
    ~date,            ~shares_equiv_current_shares,
    "2022-12-31",     1000
  ) %>% dplyr::mutate(date = as.Date(date))

  result_early <- calculate_historical_market_cap(price_data_early, financial_ts_later)
  expect_equal(nrow(result_early), 1)  # Only the row matching 2022-12-31 should remain
  expect_equal(result_early$date, as.Date("2022-12-31"))
  expect_equal(result_early$market_cap, 98000)

  # Test 4: Financial data before price data - forward-fill works as expected
  price_data_later <- tibble::tribble(
    ~date,            ~adjusted_close,
    "2022-12-31",     95
  ) %>% dplyr::mutate(date = as.Date(date))

  financial_ts_early <- tibble::tribble(
    ~date,            ~shares_equiv_current_shares,
    "2022-12-25",     950,
    "2022-12-26",     960,
    "2022-12-27",     970
  ) %>% dplyr::mutate(date = as.Date(date))

  result_later <- calculate_historical_market_cap(price_data_later, financial_ts_early)
  expect_equal(result_later$shares_equiv_current_shares_ffill, 970)
  expect_equal(result_later$market_cap, 95 * 970)

  # Test 5: Empty data frames
  empty_price <- tibble::tibble(
    date = as.Date(character()),
    adjusted_close = numeric()
  )

  empty_financials <- tibble::tibble(
    date = as.Date(character()),
    shares_equiv_current_shares = numeric()
  )

  expect_equal(
    calculate_historical_market_cap(empty_price, financial_ts_later),
    tibble::tibble(
      date = as.Date(character()),
      adjusted_close = numeric(),
      shares_equiv_current_shares_ffill = numeric(),
      market_cap = numeric()
    )
  )

  expect_equal(
    calculate_historical_market_cap(price_data_later, empty_financials),
    tibble::tibble(
      date = as.Date(character()),
      adjusted_close = numeric(),
      shares_equiv_current_shares_ffill = numeric(),
      market_cap = numeric()
    )
  )

  # Test 6: One row in financial_ts
  price_data_one_row <- tibble::tribble(
    ~date,            ~adjusted_close,
    "2022-12-31",     95,
    "2023-01-01",     96
  ) %>% dplyr::mutate(date = as.Date(date))

  financial_ts_one_row <- tibble::tribble(
    ~date,            ~shares_equiv_current_shares,
    "2022-12-31",     1000
  ) %>% dplyr::mutate(date = as.Date(date))

  result_one_row <- calculate_historical_market_cap(price_data_one_row, financial_ts_one_row)
  expect_equal(result_one_row$market_cap, c(95000, 96000))

  # Test 7: Invalid Dates - function should properly error
  price_data_invalid_date <- tibble::tribble(
    ~date,            ~adjusted_close,
    "December 30, 2023", 95,
    "2023-01-01",     96
  )

  financial_ts_dates_okay <- tibble::tribble(
    ~date,            ~shares_equiv_current_shares,
    "2022-12-30",     1000,
    "2022-12-31",     1100
  ) %>% dplyr::mutate(date = as.Date(date))

  expect_error(calculate_historical_market_cap(price_data_invalid_date, financial_ts_dates_okay))

  price_data_dates_okay <- tibble::tribble(
    ~date,            ~adjusted_close,
    "2022-12-30",     95,
    "2022-12-31",     96
  ) %>% dplyr::mutate(date = as.Date(date))

  financial_ts_dates_invalid <- tibble::tribble(
    ~date,            ~shares_equiv_current_shares,
    "December 30, 2023", 1000,
    "2023-01-01",     1100
  )

  expect_error(calculate_historical_market_cap(price_data_dates_okay, financial_ts_dates_invalid))
})
