library(SticsRFiles)

context("Compute DOY sum")


test_that("Compute DOY", {
  expect_no_error(SticsRFiles:::compute_doy_cumul(c(350, 360, 10, 20, 30), c( 1990, 1990, 1991,  1991, 1991 )))
  expect_error(SticsRFiles:::compute_doy_cumul(c(350, 370, 10, 20, 30), c( 1990, 1990, 1991,  1991, 1991 )))
 # expect_error(SticsRFiles:::compute_doy_cumul(c(350, 360, 10, 20, 30), c( 1990, 1990, 1991,  1991, 1992 )))
})
