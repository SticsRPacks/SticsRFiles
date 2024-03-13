
context("Convert calendar dates to julian dates")

test_that("same-year", {
  start_date <- as.Date("2014-08-01")
  date <- as.Date("2014-08-05")
  expected <- 4
  observed <- julian_date_for_stics(date=date, start_date=start_date)
  expect_equal(observed, expected)
})

test_that("two-successive-years_first-noleap_second-noleap", {
  start_date <- as.Date("2014-08-01")
  date <- as.Date("2015-02-10")
  expected <- 406
  observed <- julian_date_for_stics(date=date, start_date=start_date)
  expect_equal(observed, expected)
})

test_that("two-successive-years_first-noleap_second-leap", {
  start_date <- as.Date("2007-08-01")
  date <- as.Date("2008-02-10")
  expected <- 406
  observed <- julian_date_for_stics(date=date, start_date=start_date)
  expect_equal(observed, expected)
})

test_that("two-successive-years_first-leap_second-noleap", {
  start_date <- as.Date("2008-08-01")
  date <- as.Date("2009-02-10")
  expected <- 407
  observed <- julian_date_for_stics(date=date, start_date=start_date)
  expect_equal(observed, expected)
})
