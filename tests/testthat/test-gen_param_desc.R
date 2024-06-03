context("Checking nb lines of inputs.csv")
test_that("checking csv content", {
  expect_equal(dim(SticsRFiles:::get_param_desc())[1],1693)
  expect_error(SticsRFiles:::get_param_desc("xxx"))
})
