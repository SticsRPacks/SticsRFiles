library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

context("get example doc")
usm_doc <- SticsRFiles:::get_xml_doc_example("usms.xml")

test_that("get example doc", {
  expect_equal(usm_doc@name,"usms.xml")
  expect_error(SticsRFiles:::get_xml_doc_example("usm.xml"))
  expect_equal(length(unlist(SticsRFiles:::get_xml_doc_example())),8)
})
