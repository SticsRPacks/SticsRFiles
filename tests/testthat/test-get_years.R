stics_version <- get_stics_versions_compat()$latest_version

studycase_path <-
  download_data(
    example_dirs = "study_case_1",
    stics_version = stics_version,
    raise_error = TRUE
  )

workspace_path <- file.path(studycase_path, "XmlFiles")

test_that("single USM, single year", {
  expected <- data.frame(
    usm = "bou00t1",
    year1 = 2000,
    year2 = 2000
  )
  rownames(expected) <- expected$usm
  observed <- get_years(workspace_path, "usms.xml", "bou00t1")
  expect_equal(observed, expected)
})
