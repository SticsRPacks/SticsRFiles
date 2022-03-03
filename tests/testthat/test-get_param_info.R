context("searching parameter information (get_param_info)")

test_that("unknown name (should returs NULL)", {
  testthat::expect_null(get_param_info("myunknownparam"))
})

test_that("fuzzy name", {
  lai_params <-
    c(
      "lai0", "codelaitr", "codlainet", "dlaimax", "dlaimaxbrut", "dlaimin",
      "laicomp", "laiplantule", "pentlaimax", "splaimax", "splaimin", "udlaimax",
      "vlaimax", "cielclair", "codeclaircie", "juleclair", "laidebeff", "laieffeuil",
      "flai"
    )
  testthat::expect_equal(get_param_info("lai")$name, lai_params)
})

test_that("regex name", {
  testthat::expect_equal(
    get_param_info("^lai")$name,
    c(
      "lai0", "laicomp", "laiplantule", "laidebeff",
      "laieffeuil"
    )
  )
})


test_that("different stics versions", {
  v9.0 <- get_param_info("vitircarbT", stics_version = "V9.0")
  v9.2 <- get_param_info("vitircarbT", stics_version = "V9.2")

  testthat::expect_equal(attr(v9.0, "version"), "V9.0")
  testthat::expect_equal(attr(v9.2, "version"), "V9.2")

  # Set both versions to the same value for comparison:
  attr(v9.0, "version") <- attr(v9.2, "version")
  testthat::expect_equal(v9.0, v9.2)
})
