library(SticsRFiles)
options(warn=-1)
xml_usms= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.0/usms.xml"))
xml_usms_list <- c(xml_usms, xml_usms)
usms_names <- unlist(get_usms_list(xml_usms))
usms_names_list <- get_usms_list(xml_usms_list)

# Testing returned type for one or several files
context("Getting returned type, one or two files")

test_that("type, one or two files", {
  expect_is(usms_names, "list")
  expect_is(usms_names_list, "list")
})


context("checking existing usms names, one or two files")
usm_name <- "SugarCane"
test_that("name, one file", {
  #expect_true(usm_name %in% usms_names[[1]])
  expect_true(all(lapply(usms_names, function(x) usm_name %in% x)))
  expect_true(all(lapply(usms_names_list, function(x) usm_name %in% x)))
})


context("checking non-existing usms names, one or two files")
usm_name <- "UsmTest"

test_that("name, one file", {
  #expect_false(usm_name %in% usms_names[[1]])
  expect_false(all(lapply(usms_names, function(x) usm_name %in% x)))
  expect_false(all(lapply(usms_names_list, function(x) usm_name %in% x)))
})
