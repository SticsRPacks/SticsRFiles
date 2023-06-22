library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

context("Creating a varmod file to latest version")

out_dir <- file.path(tempdir(), "varmod")
if (!dir.exists(out_dir)) dir.create(out_dir)

gen_varmod(out_dir, c("lai(n)", "hauteur"))

test_that("Create a varmod file", {
  expect_true(file.exists(file.path(out_dir, "var.mod")))
  expect_warning(gen_varmod(out_dir, ""))
  expect_no_error(gen_varmod(out_dir, "hauteur",append=TRUE))
  })
gen_varmod(out_dir, "hauteur",force=TRUE)
test_that("Create a varmod file", {
  expect_true(file.exists(file.path(out_dir, "var.mod")))
})
