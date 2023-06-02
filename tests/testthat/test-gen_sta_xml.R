library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()
workspace_path <- get_examples_path("xml", stics_version = stics_version)
xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
sta_param_df <- read_params_table(file = xl_path, sheet_name = "Station")
gen_sta_xml(out_dir = workspace_path, param_df = sta_param_df)

test_that("Create a xml station file", {
  expect_true(file.exists(file.path(workspace_path, "climatex_sta.xml")))
})
