test_that("get_quarter_end_dates works with same year dates", {
  start <- as.Date("2023-01-15")
  end <- as.Date("2023-12-15")

  expected <- as.Date(c(
    "2023-03-31",
    "2023-06-30",
    "2023-09-30",
    "2023-12-31"
  ))

  result <- get_quarter_end_dates(start, end)
  expect_equal(result, expected)
})

test_that("get_quarter_end_dates works across years", {
  start <- as.Date("2023-11-15")
  end <- as.Date("2024-06-15")

  expected <- as.Date(c(
    "2023-12-31",
    "2024-03-31",
    "2024-06-30"
  ))

  result <- get_quarter_end_dates(start, end)
  expect_equal(result, expected)
})

test_that("get_quarter_end_dates handles exact quarter end dates", {
  start <- as.Date("2023-03-31")
  end <- as.Date("2023-09-30")

  expected <- as.Date(c(
    "2023-03-31",
    "2023-06-30",
    "2023-09-30"
  ))

  result <- get_quarter_end_dates(start, end)
  expect_equal(result, expected)
})



test_that("get_quarter_end_dates handles single-day ranges", {
  date <- as.Date("2023-06-15")
  expected <- as.Date("2023-06-30")

  result <- get_quarter_end_dates(date, date)
  expect_equal(result, expected)
})

test_that("get_quarter_end_dates handles first day of quarter", {
  start <- as.Date("2023-04-01")  # First day of Q2
  end <- as.Date("2023-06-30")    # Last day of Q2

  expected <- as.Date("2023-06-30")

  result <- get_quarter_end_dates(start, end)
  expect_equal(result, expected)
})

test_that("get_quarter_end_dates handles multi-year spans", {
  start <- as.Date("2022-12-15")
  end <- as.Date("2024-02-15")

  expected <- as.Date(c(
    "2022-12-31",
    "2023-03-31",
    "2023-06-30",
    "2023-09-30",
    "2023-12-31",
    "2024-03-31"
  ))

  result <- get_quarter_end_dates(start, end)
  expect_equal(result, expected)
})

test_that("get_quarter_end_dates handles leap years correctly", {
  start <- as.Date("2024-02-15")  # 2024 is a leap year
  end <- as.Date("2024-03-15")

  expected <- as.Date("2024-03-31")

  result <- get_quarter_end_dates(start, end)
  expect_equal(result, expected)
})

test_that("get_quarter_end_dates handles very long date ranges", {
  start <- as.Date("2000-01-01")
  end <- as.Date("2025-12-31")

  result <- get_quarter_end_dates(start, end)

  # Basic validation checks
  expect_true(all(diff(result) > 0))  # Dates are strictly increasing
  expect_equal(length(result), 104)    # 26 years * 4 quarters
  expect_equal(result[1], as.Date("2000-03-31"))
  expect_equal(tail(result, 1), as.Date("2025-12-31"))
})

test_that("get_quarter_end_dates validates inputs", {
  expect_error(
    get_quarter_end_dates("2023-01-01", as.Date("2023-12-31")),
    "Both start_date and end_date must be Date objects"
  )

  expect_error(
    get_quarter_end_dates(as.Date("2023-12-31"), as.Date("2023-01-01")),
    "start_date must be before or equal to end_date"
  )
})
