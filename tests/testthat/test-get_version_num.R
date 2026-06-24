test_that("Get version number as svlist object", {
  expect_s3_class(get_version_num("10"), 'svlist')
  expect_s3_class(get_version_num("V10"), "svlist")
  expect_s3_class(get_version_num("v10"), "svlist")
  expect_s3_class(get_version_num(10), "svlist")
  ver <- get_version_num("10")
  expect_equal(ver, get_version_num("10.0"))
  expect_equal(ver, get_version_num("10.0.0"))
  expect_equal(ver, get_version_num(10))
})

test_that("Get version number as character object", {
  expect_true(inherits(get_version_num("10", numeric = FALSE), "character"))
  expect_true(inherits(get_version_num(10, numeric = FALSE), "character"))
  ver <- get_version_num("10", numeric = FALSE)
  expect_identical(ver, get_version_num("10.0", numeric = FALSE))
  expect_identical(ver, get_version_num("v10.0", numeric = FALSE))
  expect_identical(ver, get_version_num("V10.0", numeric = FALSE))
  expect_identical(ver, get_version_num("10.0.0", numeric = FALSE))
  expect_identical(ver, get_version_num(10, numeric = FALSE))
})


test_that("Comparing versions with svlist objects", {
  expect_true(get_version_num("11") > get_version_num("10"))
  expect_true(get_version_num("v11") > get_version_num("v10"))
  expect_true(get_version_num("V11") > get_version_num("V10"))
  expect_true(get_version_num("11") > get_version_num("10.1"))
  expect_true(get_version_num(11) > get_version_num(10))
  expect_true(get_version_num(11) > get_version_num(10.1))
})
