context("Creating a varmod file to latest version")

out_dir <- file.path(tempdir(), "varmod")
if (!dir.exists(out_dir)) dir.create(out_dir)

gen_varmod(out_dir, "lai(n)")

test_that("Create a varmod file", {
  expect_true(file.exists(file.path(out_dir, "var.mod")))
})

gen_varmod(out_dir, "hauteur", append = TRUE)

test_that("Append a new variable", {
  expect_length(
    grep(
      pattern = "hauteur",
      readLines(file.path(out_dir, "var.mod"))
    ),
    1
  )
  expect_length(
    grep(
      pattern = "lai(n)",
      readLines(file.path(out_dir, "var.mod")),
      fixed = TRUE
    ),
    1
  )
  expect_message(gen_varmod(out_dir, "", verbose = TRUE))
})


test_that("Append a unknown variable", {
  gen_varmod(out_dir, "test", append = TRUE)
  expect_length(
    grep(
      pattern = "test",
      readLines(file.path(out_dir, "var.mod"))
    ),
    0
  )
  gen_varmod(out_dir, "test", append = TRUE, force = TRUE)
  expect_length(
    grep(
      pattern = "test",
      readLines(file.path(out_dir, "var.mod"))
    ),
    1
  )
  expect_no_message(gen_varmod(out_dir, "test", force = TRUE))
})
