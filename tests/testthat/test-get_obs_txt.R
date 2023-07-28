library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Read obs file")

workspace_path <- get_examples_path("obs", stics_version = stics_version)

test_that("Read obs file", {
  expect_equal(
    unlist(
      dplyr::count(get_obs_txt(file.path(workspace_path, "simple_example"))),
      use.names = FALSE),
    37)
  expect_warning(
    unlist(
      get_obs_txt(file.path(workspace_path, "simple_example"),
                  mixed = TRUE))
    )
})
