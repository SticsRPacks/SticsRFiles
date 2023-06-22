library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

context("Get parameter names for an option choice value")
xml_path <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")

SticsRFiles:::get_option_choice_param_values(xml_path, "codetemp", "2")
test_that("Option choice", {
  expect_equal(length(unlist(SticsRFiles:::get_option_choice_param_values(xml_path, "codetemp", "2"),use.names = FALSE)),14)
  #expect_warning(unlist(get_obs_txt(file.path(workspace_path,"simple_example"),mixed=TRUE)))
})
