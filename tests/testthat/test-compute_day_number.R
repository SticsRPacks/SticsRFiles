
context("Convert calendar dates to julian dates")

test_that("same-year", {
  #start_date <- as.Date("2014-08-01")
  date <- as.Date("2014-08-10")
  expected <- 222
  #observed <- compute_day_from_date(date=date, start_date=start_date)
  observed <- compute_day_from_date(date = date)
  expect_equal(observed, expected)

  observed <- compute_day_from_date(date = date, start_year = 2014)
  expect_equal(observed, expected)

  date <- as.Date(c("2014-08-10", "2014-08-20"))
  expected <- c(222, 232)
  #observed <- compute_day_from_date(date=date, start_date=start_date)
  observed <- compute_day_from_date(date = date)
  expect_equal(observed, expected)

  observed <- compute_day_from_date(date = date, start_year = 2014)
  expect_equal(observed, expected)

})

test_that("two-successive-years_first-noleap_second-noleap", {
  #start_date <- as.Date("2014-08-01")
  date <- as.Date("2015-02-10")
  expected <- 406
  # observed <- compute_day_from_date(date=date, start_date=start_date)
  observed <- compute_day_from_date(date=date, start_year = 2014)
  expect_equal(observed, expected)

  date <- as.Date(c("2015-02-10", "2015-02-20"))
  expected <- c(406, 416)
  #observed <- compute_day_from_date(date=date, start_date=start_date)
  observed <- compute_day_from_date(date=date, start_year = 2014)
  expect_equal(observed, expected)
})

test_that("two-successive-years_first-noleap_second-leap", {
  #start_date <- as.Date("2007-08-01")
  date <- as.Date("2008-02-10")
  expected <- 406
  #observed <- compute_day_from_date(date=date, start_date=start_date)
  observed <- compute_day_from_date(date = date, start_year = 2007)
  expect_equal(observed, expected)

  date <- as.Date(c("2008-02-10", "2008-02-20"))
  expected <- c(406, 416)
  #observed <- compute_day_from_date(date=date, start_date=start_date)
  observed <- compute_day_from_date(date = date, start_year = 2007)
  expect_equal(observed, expected)
})

test_that("two-successive-years_first-leap_second-noleap", {
  #start_date <- as.Date("2008-08-01")
  date <- as.Date("2009-02-10")
  expected <- 407
  #observed <- compute_day_from_date(date=date, start_date=start_date)
  observed <- compute_day_from_date(date = date, start_year = 2008)
  expect_equal(observed, expected)

  date <- as.Date(c("2009-02-10", "2009-02-20"))
  expected <- c(407, 417)
  #observed <- compute_day_from_date(date = date, start_date=start_date)
  observed <- compute_day_from_date(date = date, start_year = 2008)
  expect_equal(observed, expected)
})
