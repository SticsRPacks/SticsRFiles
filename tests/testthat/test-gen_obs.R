xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
obs_df <- read_params_table(file = xl_path, sheet_name = "Obs")
context("Create obs file ")

test_that("Obs file", {
  expect_true(gen_obs(df = obs_df, out_dir = get_examples_path("xl")))
  expect_warning(gen_obs(df = obs_df, out_dir = "xxx"))
})

obs_sd_df <- read_params_table(file = xl_path, sheet_name = "Obs_SD")
context("Create obs sd file ")

test_that("Obs sd file", {
  expect_true(gen_obs_sd(df = obs_sd_df, out_dir = get_examples_path("xl")))
  expect_warning(gen_obs_sd(df = obs_sd_df, out_dir = "xxx"))
})
