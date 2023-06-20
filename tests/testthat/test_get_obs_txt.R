library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

context("Counting rows of climate file")

workspace_path <- get_examples_path("obs", stics_version = stics_version)
obs_table <- SticsRFiles:::get_obs_txt(file.path(workspace_path,"simple_example"))
test_that("Count rows climate file", {
expect_equal(unlist(dplyr::count(obs_table),use.names=FALSE),37)
})
