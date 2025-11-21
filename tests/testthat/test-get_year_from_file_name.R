stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Get years from files name")

test_that("single file path", {
  expect_equal(get_year_from_file_name("abc.2000"), 2000)
})

test_that("single file path no year in file name", {
  expect_true(is.na(get_year_from_file_name("abc.def")))
})

test_that("vector of file paths", {
  years <- get_year_from_file_name(c("abc.2000", "abc.1999"))
  expect_equal(length(years), 2)
  expect_equal(years[1], 2000)
  expect_equal(years[2], 1999)
})
