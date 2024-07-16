stics_version <- get_stics_versions_compat()$latest_version
context("Checking generating solxsl file")
test_that("checking if exists", {
  expect_true(SticsRFiles:::gen_sol_xsl_file("solcanne", stics_version ))
})
