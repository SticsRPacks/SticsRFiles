library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

workspace_path <- get_examples_path("xml", stics_version = stics_version)
javastics_path <- workspace_path

xml_usms <- file.path(workspace_path, "usms.xml")

usm_files<-SticsRFiles:::check_usms_files(
  workspace_path,
  javastics_path,
  usms_list = NULL,
  file_name = "usms.xml"
)

context("Getting returned type for usm names")

test_that("type, character vector", {
  expect_is(usm_files$usm, "character")
})

context("checking existing usm names, one or two files")
usm_name <- "flax"
test_that("name, one file", {
  expect_true(usm_name %in% usm_files$usm)
})

context("checking existing usm names, one or two files")
usm_name <- "brrr"
test_that("name, one file", {
  expect_false(usm_name %in% usm_files$usm)
})
