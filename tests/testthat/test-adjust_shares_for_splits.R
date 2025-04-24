test_that("share split adjustments (for comparable metrics) are calculated correctly", {
  # Setup
  # Basic case with two forward splits
  financial_data_basic <- tibble::tribble(
    ~date,            ~shares_outstanding,
    "1998-12-31",     1000,
    "1999-03-31",     1000,
    "1999-06-30",     1000,
    "1999-12-31",     2000 # Assume issuance happened sometime in Q4
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  splits_basic <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "1999-01-05",      3,
    "1999-09-02",      2
  ) %>%
    dplyr::mutate(effective_date = lubridate::as_date(effective_date))

  # Test 1: Basic forward splits
  result_basic <- adjust_shares_for_splits(financial_data_basic, splits_basic)

  # Adjust historical shares to be equivalent to current share basis
  # 1998-12-31: 1000 shares reported. Split 1 (x3) and Split 2 (x2) happened AFTER this date. Adjusted: 1000 * 3 * 2 = 6000
  # 1999-03-31: 1000 shares reported. Split 1 happened BEFORE this date. Split 2 (x2) happened AFTER. Adjusted: 1000 * 2 = 2000
  # 1999-06-30: 1000 shares reported. Split 1 happened BEFORE this date. Split 2 (x2) happened AFTER. Adjusted: 1000 * 2 = 2000
  # 1999-12-31: 2000 shares reported. Both splits happened BEFORE this date. Adjusted: 2000 * 1 = 2000
  expect_equal(
    result_basic$shares_equiv_current_shares,
    c(6000, 2000, 2000, 2000)
  )
  # Check original shares are untouched
  expect_equal(
    result_basic$shares_outstanding,
    financial_data_basic$shares_outstanding
  )

  # Complex case with multiple splits including reverse split
  financial_data_complex <- tibble::tribble(
    ~date,            ~shares_outstanding,
    "2020-01-01",     10000,
    "2020-06-30",     10000,
    "2021-01-01",     20000, # Issuance occurred
    "2021-06-30",     20000,
    "2022-01-01",     40000  # Issuance occurred
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  splits_complex <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2020-03-15",      2,    # S1
    "2020-09-01",      3,    # S2
    "2021-03-01",      0.5,  # S3 (Reverse)
    "2021-12-01",      4     # S4
  ) %>%
    dplyr::mutate(effective_date = lubridate::as_date(effective_date))

  # Test 2: Complex forward and reverse splits
  result_complex <- adjust_shares_for_splits(financial_data_complex, splits_complex)

  # Calculations for shares_equiv_current_shares:
  # 2020-01-01 (Before S1, S2, S3, S4): 10000 * 2 * 3 * 0.5 * 4 = 120000
  # 2020-06-30 (After S1; Before S2, S3, S4): 10000 * 3 * 0.5 * 4 = 60000
  # 2021-01-01 (After S1, S2; Before S3, S4): 20000 * 0.5 * 4 = 40000
  # 2021-06-30 (After S1, S2, S3; Before S4): 20000 * 4 = 80000
  # 2022-01-01 (After S1, S2, S3, S4): 40000 * 1 = 40000
  expect_equal(
    result_complex$shares_equiv_current_shares,
    c(120000, 60000, 40000, 80000, 40000)
  )
   # Check original shares are untouched
  expect_equal(
    result_complex$shares_outstanding,
    financial_data_complex$shares_outstanding
  )


  # Test 3: Empty splits data - should add new column equal to original
  empty_splits <- tibble::tribble(
    ~effective_date, ~split_factor
    # schema definition only
  ) %>% dplyr::mutate(effective_date = as.Date(effective_date), split_factor = as.numeric(split_factor))


  result_no_splits <- adjust_shares_for_splits(financial_data_basic, empty_splits)

  expect_equal(
    result_no_splits$shares_equiv_current_shares,
    result_no_splits$shares_outstanding
  )
  expect_true("shares_equiv_current_shares" %in% names(result_no_splits))


  # Test 4: Invalid split factors
  invalid_splits_zero <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2020-01-01",      0
  ) %>%
    dplyr::mutate(effective_date = lubridate::as_date(effective_date))

  invalid_splits_neg <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2020-02-01",      -1
  ) %>%
    dplyr::mutate(effective_date = lubridate::as_date(effective_date))

  expect_error(
    adjust_shares_for_splits(financial_data_basic, invalid_splits_zero),
    "Split factors must be positive numbers"
  )
   expect_error(
    adjust_shares_for_splits(financial_data_basic, invalid_splits_neg),
    "Split factors must be positive numbers"
  )

  # Test 5: Unordered dates in input data (should be sorted internally)
  unordered_financial <- tibble::tribble(
    ~date,            ~shares_outstanding,
    "2020-03-15",     1000,  # Later date
    "2020-01-15",     1000   # Earlier date
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  unordered_splits <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2020-02-15",      2, # S2
    "2020-01-30",      3  # S1
  ) %>%
    dplyr::mutate(effective_date = lubridate::as_date(effective_date))

  result_unordered <- adjust_shares_for_splits(unordered_financial, unordered_splits)

  # Internal sorting should happen first for both dataframes.
  # Financial dates: 2020-01-15, 2020-03-15
  # Split dates: 2020-01-30 (S1, x3), 2020-02-15 (S2, x2)
  # Calculations:
  # 2020-01-15 (Before S1, S2): 1000 * 3 * 2 = 6000
  # 2020-03-15 (After S1, S2): 1000 * 1 = 1000
  expect_equal(
    result_unordered$shares_equiv_current_shares,
    c(6000, 1000) # Order matches the sorted financial data dates
  )
  expect_equal(result_unordered$date, sort(unordered_financial$date))


  # Test 6: Split coincides exactly with a financial statement date
  coinciding_financial <- tibble::tribble(
    ~date,            ~shares_outstanding,
    "2020-01-15",     1000,
    "2020-02-15",     2000,  # Same date as split, assume report reflects post-split state conceptually for *this* report date
    "2020-03-15",     2000
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  coinciding_splits <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2020-02-15",      2 # S1
  ) %>%
    dplyr::mutate(effective_date = lubridate::as_date(effective_date))

  result_coinciding <- adjust_shares_for_splits(coinciding_financial, coinciding_splits)

  # Calculation: Adjustment only applies if financial date < split date
  # 2020-01-15 (Before S1): 1000 * 2 = 2000
  # 2020-02-15 (Equal to S1): 2000 * 1 = 2000 (date is NOT < split_date)
  # 2020-03-15 (After S1): 2000 * 1 = 2000
  expect_equal(
    result_coinciding$shares_equiv_current_shares,
    c(2000, 2000, 2000)
  )

  # Test 7: Splits with dates outside the financial data range
  outside_financial <- tibble::tribble(
    ~date,            ~shares_outstanding,
    "2021-01-01",     5000,
    "2021-06-01",     5000
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  outside_splits <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2019-01-01",      2,  # Split before all financial data -> No effect
    "2022-01-01",      3   # Split after all financial data -> Affects all rows
  ) %>%
    dplyr::mutate(effective_date = lubridate::as_date(effective_date))

  result_outside <- adjust_shares_for_splits(outside_financial, outside_splits)

  # Calculation: Only splits *after* the financial date matter
  # 2021-01-01: Before 2022-01-01 split. Affected by x3. 5000 * 3 = 15000
  # 2021-06-01: Before 2022-01-01 split. Affected by x3. 5000 * 3 = 15000
  expect_equal(
    result_outside$shares_equiv_current_shares,
    c(15000, 15000)
  )

  # Test 8: Ensure Date types are handled correctly
  financial_data_chr <- tibble::tribble(
    ~date,            ~shares_outstanding,
    "2020-12-31",     1000,
    "2021-12-31",     1000
  )

  splits_data_chr <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2021-06-01",      2
  )

  expect_no_error(adjust_shares_for_splits(financial_data_chr, splits_data_chr))
  result_chr <- adjust_shares_for_splits(financial_data_chr, splits_data_chr)
  expect_equal(result_chr$shares_equiv_current_shares, c(2000, 1000))

  # Test 9: Error if dates cannot be coerced
  financial_data_bad_date <- tibble::tribble(
    ~date,            ~shares_outstanding,
    "Dec 31 2020",     1000,
    "2021-12-31",     1000
  )
   splits_data_ok_date <- tibble::tribble(
    ~effective_date,   ~split_factor,
    "2021-06-01",      2
    ) %>% dplyr::mutate(effective_date = as.Date(effective_date))

  expect_error(adjust_shares_for_splits(financial_data_bad_date, splits_data_ok_date), "financial_data\\$date cannot be coerced to Date")


})
