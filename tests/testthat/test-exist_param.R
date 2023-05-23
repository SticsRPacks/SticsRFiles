library(SticsRFiles)

stics_version <- SticsRFiles:::get_stics_versions_compat()$latest_version
xml_sols <- file.path(get_examples_path(file_type = "xml"), "sols.xml")

xml_doc <- SticsRFiles:::xmldocument(xml_sols)

SticsRFiles:::exists_param(xml_doc, "cfes")
SticsRFiles:::exists_param(xml_doc, c("cfes", "mulchbat"))
context("Exist param ")

test_that("exist parameter", {
  expect_true(SticsRFiles:::exists_param(xml_doc, "cfes"))
  expect_vector(SticsRFiles:::exists_param(xml_doc, c("cfes", "mulchbat")),ptype=NULL,size=2)

})
