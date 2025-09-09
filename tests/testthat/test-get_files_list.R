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

xml_usms <- file.path(workspace_path, "usms.xml")

usms_list <- get_usms_list(xml_usms)

usms_files <- get_files_list(workspace_path)

context("Checking usm names in list")
test_that("checking usm exists", {
  expect_true("bo96iN+" %in% names(usms_files))
})

context("Checking all list usm names against usms file")
test_that("checking usm consistency", {
  expect_true(all(usms_list %in% names(usms_files)))
})

context("checking wrong usm name")
test_that("one name", {
  expect_false("brrr" %in% names(usms_files))
})

context("testing returned object type")
test_that("is list", {
  expect_true(is.list(usms_files))
})

context("Checking list fields types")
test_that("exist, paths", {
  usms_files <- get_files_list(workspace_path)
  expect_is(usms_files[[1]]$exist, "logical")
  expect_is(usms_files[[1]]$paths, "character")
})

context("Checking if mod files exist")

test_that("mod files exist in files", {
  usms_files <- get_files_list(workspace_path, use_mod_files = TRUE)
  expect_true(
    length(
      grep(
        pattern = "\\.mod$",
        x = usms_files[["bo96iN+"]][["paths"]]
      )
    ) >
      0
  )
})

test_that("mod files do not exist in files", {
  usms_files <- get_files_list(workspace_path)
  expect_true(
    length(
      grep(
        pattern = "\\.mod$",
        x = usms_files[["bo96iN+"]][["paths"]]
      )
    ) ==
      0
  )
})

# add tests for getting a sublist according to file types
context("Getting a sublist according to file type")
test_that("finit files", {
  usms_files <- get_files_list(workspace_path, file_type = c("finit"))
  expect_true(
    length(
      grep(
        pattern = "\\_ini.xml$",
        x = usms_files[["bo96iN+"]][["paths"]]
      )
    ) >
      0 &
      length(usms_files[["bo96iN+"]][["paths"]]) == 1
  )
})
