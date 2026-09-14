context("Test version management")

test_that("latest version", {
  expect_equal(check_version(), get_latest_version())
})


test_that("other versions", {
  # with V
  stics_version <- "V10"
  expect_equal(check_version(stics_version), stics_version)
  # without V
  stics_version <- "10"
  expect_equal(check_version(stics_version), stics_version)
  expect_error(check_version("V12"), "V12")
})


test_that("versions components", {
  expect_equal(get_major_version("V10"), 10)
})
