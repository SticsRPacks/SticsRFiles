stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()
host <- "github.com"

studycase_path <-
  download_data(
    example_dirs = "study_case_1",
    stics_version = stics_version,
    raise_error = TRUE
  )

workspace_path <- file.path(studycase_path, "XmlFiles")

test_that("single USM, single year", {
  expected <- data.frame(
    usm = "bo96iN+",
    year1 = 1996,
    year2 = 1996
  )
  rownames(expected) <- expected$usm
  observed <- get_years(workspace_path, "usms.xml", "bo96iN+")
  expect_equal(observed, expected)
})
