library(SticsRFiles)
options(warn=-1)
xml_usms= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.1/usms.xml"))
usms_names <- get_usms_list(usm_path = xml_usms)

# Testing returned type for one or several files
context("Getting returned type")

test_that("type, character vestor", {
  expect_is(usms_names, "character")
})


context("checking existing usms names, one or two files")
usm_name <- "SugarCane"
test_that("name, one file", {
  expect_true(usm_name %in% usms_names)
  #expect_true(all(lapply(usms_names, function(x) usm_name %in% x)))
})


context("checking non-existing usms names, one or two files")
usm_name <- "UsmTest"

test_that("name, one file", {
  expect_false(usm_name %in% usms_names)
  #expect_false(all(lapply(usms_names, function(x) usm_name %in% x)))
})
