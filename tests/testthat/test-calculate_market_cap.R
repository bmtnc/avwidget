library(testthat)

test_that("basic market cap calculations work with no splits", {
  price_data_basic <- tibble::tribble(
    ~date,          ~close,  ~adjusted_close,
    "2023-01-01",   100,     100,
    "2023-02-01",   102,     102,
    "2023-03-01",   98,      98,
    "2023-04-01",   103,     103,
    "2023-05-01",   101,     101,
    "2023-06-01",   99,      99,
    "2023-07-01",   104,     104,
    "2023-08-01",   102,     102,
    "2023-09-01",   100,     100,
    "2023-10-01",   105,     105,
    "2023-11-01",   103,     103,
    "2023-12-01",   101,     101
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  # For simplicity, use the same share count for all dates
  shares <- rep(1000000, nrow(price_data_basic))
  
  # Calculate market cap for all dates
  market_caps <- calculate_market_cap(price_data_basic$close, shares)
  
  # Test the first value
  expect_equal(market_caps[1], 0.1)
})

test_that("market cap calculations handle 2:1 split correctly", {
  price_data_split <- tibble::tribble(
    ~date,          ~close,  ~adjusted_close,
    "2023-01-01",   200,     100,
    "2023-02-01",   210,     105,
    "2023-03-01",   220,     110,
    "2023-04-01",   230,     115,
    "2023-05-01",   240,     120,
    "2023-06-01",   250,     125,
    "2023-07-01",   125,     125,
    "2023-08-01",   130,     130,
    "2023-09-01",   135,     135,
    "2023-10-01",   140,     140,
    "2023-11-01",   145,     145,
    "2023-12-01",   150,     150
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  # Before June's split, shares need to be adjusted
  # After the split, shares are the same
  shares <- c(
    rep(2000000, 6),  # Jan-Jun: 2M shares (already adjusted for future split)
    rep(2000000, 6)   # Jul-Dec: 2M shares (actual shares after split)
  )
  
  # Calculate market cap for all dates
  market_caps <- calculate_market_cap(price_data_split$close, shares)
  
  # Test specific values
  expect_equal(market_caps[6], 0.5)  # June: 250 * 2M = 500M
  expect_equal(market_caps[7], 0.25) # July: 125 * 2M = 250M
})

test_that("market cap calculations handle AMZN 20:1 split correctly", {
  price_data_amzn <- tibble::tribble(
    ~date,          ~close,   ~adjusted_close,
    "2022-01-01",   3000,     150,
    "2022-02-01",   3000,     150,
    "2022-03-01",   3000,     150,
    "2022-04-01",   3000,     150,
    "2022-05-01",   3000,     150,
    "2022-06-03",   3000,     150,
    "2022-06-06",   150,      150,
    "2022-07-01",   150,      150,
    "2022-08-01",   150,      150,
    "2022-09-01",   150,      150,
    "2022-10-01",   150,      150,
    "2022-12-01",   150,      150
  ) %>%
    dplyr::mutate(date = lubridate::as_date(date))

  # Use 500M shares before the split (adjusted to 10B after)
  # and 10B shares after the split
  shares <- c(
    rep(500000000, 6),  # Pre-split periods: 500M historical shares
    rep(10000000000, 6) # Post-split periods: 10B shares
  )
  
  # Calculate correct market caps before and after split
  market_caps <- calculate_market_cap(price_data_amzn$close, shares)
  
  # Test specific values
  expect_equal(market_caps[6], 1500)  # June 3: 3000 * 500M = 1.5T
  expect_equal(market_caps[7], 1500)  # June 6: 150 * 10B = 1.5T
})

test_that("market cap calculations fail gracefully with invalid inputs", {
  expect_error(
    calculate_market_cap("100", c(1000000)),
    "must be numeric"
  )
  
  expect_error(
    calculate_market_cap(c(100, 200), c(1000000)),
    "same length"
  )
})
