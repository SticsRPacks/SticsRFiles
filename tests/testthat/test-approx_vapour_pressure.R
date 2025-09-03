context("Approximate vapour pressure")

test_that("a-single-input", {
  TM <- 10
  RH <- 75
  expected <- RH * (6.1070*(1 + sqrt(2)*sin(0.017453293*TM/3))^8.827) / 100
  observed <- approx_vapour_pressure(TM, RH)
  expect_equal(observed, expected)
})

test_that("several-inputs", {
  TM <- c(10, 20)
  RH <- c(75, 50)
  expected <- RH * (6.1070*(1 + sqrt(2)*sin(0.017453293*TM/3))^8.827) / 100
  observed <- approx_vapour_pressure(TM, RH)
  expect_equal(observed, expected)
})

test_that("several-inputs_with-NA", {
  TM <- c(10, NA, 20)
  RH <- c(75, NA, 50)
  expected <- RH * (6.1070*(1 + sqrt(2)*sin(0.017453293*TM/3))^8.827) / 100
  observed <- approx_vapour_pressure(TM, RH)
  expect_equal(observed, expected)
})
