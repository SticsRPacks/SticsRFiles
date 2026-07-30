versions <- get_stics_versions_compat()
latest <- get_stics_versions_compat()$latest_version

test_that("latest version", {
  expect_equal(versions$versions_list[length(versions$versions_list)], latest)
})

test_that("latest version", {
  expect_equal(get_stics_versions_compat(0), latest)
  expect_equal(
    get_stics_versions_compat(-1),
    versions$versions_list[length(versions$versions_list) - 1]
  )
  expect_equal(
    get_stics_versions_compat(1),
    versions$versions_list[1]
  )
})
