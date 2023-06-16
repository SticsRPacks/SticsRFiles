library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

context("Reaserching parameter bounds")

xml_file <- file.path(get_examples_path("xml", stics_version = stics_version),"sols.xml")
xml_doc <- SticsRFiles:::xmldocument(xml_file)
test_that("Reaserching parameter bounds", {
   expect_equal(length(get_param_bounds(xml_doc, "profhum", "min")),2)
   expect_equal(length(get_param_bounds(xml_doc, "profhum")),3)
   expect_warning(length(get_param_bounds(xml_doc, "profhum")))
   expect_equal(length(get_param_bounds(xml_doc, "profhum",c("min", "max"))),3)
})


