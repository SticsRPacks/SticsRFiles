context("Get years number for a usm from files name")

data_dir <- download_data(
  example_dirs = "study_case_1",
  raise_error = TRUE
)

workspace <- file.path(data_dir, "XmlFiles")

files_path <- file.path(
  workspace,
  c("lusignaj.1996", "lusignaj.1997", "lusignaj.abcd")
)
file.copy(files_path[2], files_path[3])

test_that("single file path", {
  expect_error(get_years_number(files_path[1]))
})

test_that("2 files path, no year in one file name", {
  expect_error(get_years_number(files_path[2:3]))
})

test_that("vector of file paths", {
  expect_equal(get_years_number(files_path[1:2]), 2)
  expect_equal(get_years_number(rep(files_path[1], 2)), 1)
})
