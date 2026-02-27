stics_version <- get_stics_versions_compat()$latest_version

context("Counting rows of climate file")

workspace_path <- get_examples_path("txt", stics_version = stics_version)

meteo <- get_climate_txt(workspace_path)

test_that("Count rows climate file", {
  expect_equal(nrow(meteo), 366)
})
