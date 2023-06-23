library(SticsRFiles)

context("Compute DOY sum")


test_that("Compute DOY", {
  expect_equal(
    compute_doy_cumul(c(350, 360, 10, 20, 30),
                      c(1990, 1990, 1991,  1991, 1991)),
    c(350, 360, 375, 385, 395)
    )
  expect_error(
    compute_doy_cumul(c(350, 370, 10, 20, 30),
                      c(1990, 1990, 1991,  1991, 1991))
    )
})
