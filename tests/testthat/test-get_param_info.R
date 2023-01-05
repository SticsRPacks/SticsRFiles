context("searching parameter information (get_param_info)")


stics_version <- get_stics_versions_compat()$latest_version

test_that("unknown name (should return NULL)", {
  testthat::expect_null(get_param_info("myunknownparam"))
})

test_that("fuzzy name", {
  if (SticsRFiles:::get_version_num() < 10) {
    lai_params <-
      c(
        "lai0", "codelaitr", "codlainet", "dlaimax", "dlaimaxbrut", "dlaimin",
        "laicomp", "laiplantule", "pentlaimax", "splaimax", "splaimin",
        "udlaimax", "vlaimax", "cielclair", "codeclaircie", "juleclair",
        "laidebeff", "laieffeuil", "lairesiduel", "flai"
      )
  } else {
    lai_params <-
      c(
        "lai0", "codelaitr", "codlainet", "dlaimax", "dlaimaxbrut", "dlaimin",
        "inilai", "laicomp", "laiplantule", "pentlaimax", "splaimax",
        "splaimin", "udlaimax", "vlaimax", "cielclair", "codeclaircie",
        "juleclair", "laidebeff", "laieffeuil", "lairesiduel", "flai"
      )
  }
  testthat::expect_equal(get_param_info("lai",
    stics_version = stics_version
  )$name, lai_params)
})

test_that("regex name", {
  testthat::expect_equal(
    get_param_info("^lai")$name,
    c(
      "lai0", "laicomp", "laiplantule", "laidebeff", "laieffeuil", "lairesiduel"
    )
  )
})


test_that("different stics versions", {
  v_90 <- get_param_info("vitircarbT", stics_version = "V9.0")
  v_92 <- get_param_info("vitircarbT", stics_version = "V9.2")
  vlast <- get_param_info("vitircarbT", stics_version = stics_version)

  testthat::expect_equal(attr(v_90, "version"), "V9.0")
  testthat::expect_equal(attr(v_92, "version"), "V9.2")
  testthat::expect_equal(attr(vlast, "version"), stics_version)

  # Set both versions to the same value for comparison:
  attr(v_90, "version") <- attr(v_92, "version")
  testthat::expect_equal(v_90, v_92)
  # breaking, parameter moved to cultivar parameters since V9.2
  testthat::expect_false(identical(vlast, v_92))
})
