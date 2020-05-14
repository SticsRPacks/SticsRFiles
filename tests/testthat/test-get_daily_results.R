context("get model outputs")

path <- get_examples_path(file_type = "sti")


test_that("output is a data.frame", {
  outputs= get_daily_results(path,"banana")
  expect_true(is.data.frame(outputs))
})

test_that("get output without usm argument", {
  outputs= get_daily_results(workspace = path)
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(is.data.frame(outputs$banana))
  expect_true(is.null(outputs$proto_rice))
})
