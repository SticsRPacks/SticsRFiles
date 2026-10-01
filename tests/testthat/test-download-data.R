context("Test downloading use case data from the data repository")

test_that("getting referenced directories", {
  expect_false(is.null(get_referenced_dirs()))
  expect_equal(get_referenced_dirs(), c("study_case_1", "study_case_intercrop"))
  expect_true(is.null(get_referenced_dirs(dirs = c("a", "b"))))
  expect_false(is.null(get_referenced_dirs(stics_version = "V9.2")))
  expect_true(is.null(get_referenced_dirs(stics_version = "V12.0")))
})


test_that("get data url", {
  expect_false(is.null(get_data_url()))
  expect_null(get_data_url("branch"))
})


test_that("downloading data", {
  expect_null(download_data("branch"))
  expect_null(download_data(example_dirs = c("a", "b")))
  expect_null(download_data(stics_version = "V12.0"))
  expect_error(download_data(branch = "branch", raise_error = TRUE))
  expect_error(download_data(example_dirs = c("a", "b"), raise_error = TRUE))
  expect_error(download_data(stics_version = "V12.0", raise_error = TRUE))

  # captured <- new.env(parent = emptyenv())
  #
  # local_mocked_bindings(
  #   get_data_url = function(branch) {
  #     captured$branch <- branch
  #     invisible(list())
  #   }
  # )
  # invisible(get_data_url(branch))
  # expect_equal(captured$branch, "master")
})
