context("get model outputs")

path <- system.file(file.path("extdata","sti","V9.0"), package = "SticsRFiles")



test_that("output is a data.frame", {
  outputs= get_daily_results(path,"banana")
  expect_true(is.data.frame(outputs))
})

test_that("get output without usm argument", {
  outputs= get_daily_results(workspace = path)
  expect_true(is.data.frame(outputs))
})
