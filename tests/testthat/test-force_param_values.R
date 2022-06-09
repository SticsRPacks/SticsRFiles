library(SticsRFiles)


example_txt_dir <- get_examples_path(file_type = "txt", stics_version = "V9.2")
res <- force_param_values(example_txt_dir, values = setNames(object = c(220, 330), c("stlevamf", "stamflax")))
df_paramsti <- read.table(file = file.path(example_txt_dir, "param.sti"), stringsAsFactors = FALSE)

# to be compatible with both STICS V10.0 (codoptim) and previous version (codeoptim)
# we search all parameters including the pattern "optim" and then handle both cases in the test
optim_params <- get_param_txt(workspace = example_txt_dir, param = "optim")

test_that("standard case", {
  expect_equal(optim_params[[grep("cod.optim",names(optim_params))]], "1")
  expect_equal(df_paramsti[c(2, 4), ], c("stlevamf", "stamflax"))
  expect_equal(as.numeric(df_paramsti[c(1, 3, 5), ]), c(2, 220, 330))
  expect_true(res)
})



res <- force_param_values(example_txt_dir, values = NA)

test_that("param_values == NA", {
  expect_equal(get_param_txt(workspace = example_txt_dir, param = "codeoptim"), c(usm.codeoptim = "0"))
  expect_false(file.exists(file.path(example_txt_dir, "param.sti")))
  expect_true(res)
})



res <- suppressWarnings(force_param_values(example_txt_dir, values = setNames(object = c(220, NA), c("stlevamf", "stamflax"))))
df_paramsti <- read.table(file = file.path(example_txt_dir, "param.sti"), stringsAsFactors = FALSE)

test_that("One NA in param_values", {
  expect_equal(get_param_txt(workspace = example_txt_dir, param = "codeoptim"), c(usm.codeoptim = "1"))
  expect_equal(df_paramsti[c(2), ], c("stlevamf"))
  expect_equal(as.numeric(df_paramsti[c(1, 3), ]), c(1, 220))
  expect_true(res)
})
