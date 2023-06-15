library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

#context("Creating new_travail.usm file to latest version")

workspace_path <- get_examples_path("xml", stics_version = stics_version)
context("Exist Javasitcs ")

test_that("exist Javasticsr", {
  expect_error(check_java_path(workspace_path))
  # expect_true(exist_param_xml("codegdh", stics_version = stics_version))
  # expect_true(exist_param_xml("codephot", stics_version = stics_version))
  # expect_warning(exist_param_xml("codexxx", stics_version = stics_version))
  # expect_true(exist_param_xml("code_acti_reserve",
  #                             stics_version = stics_version))
})
