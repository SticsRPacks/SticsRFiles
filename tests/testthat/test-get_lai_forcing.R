stics_version <- get_stics_versions_compat()$latest_version

context("Test LAI Forcing")

workspace_path <- get_examples_path("xml", stics_version = stics_version)
xml_usms <- file.path(workspace_path, "usms.xml")
get_lai_forcing(xml_usms, "wheat")

test_that("Lai Forcing", {
  expect_false(get_lai_forcing(xml_usms, "wheat"))
})
