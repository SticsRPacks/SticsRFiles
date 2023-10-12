library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- SticsRFiles:::get_version_num()

tec_xml <- file.path(get_examples_path(file_type = "xml"), "file_tec.xml")
tec_doc <- SticsRFiles:::xmldocument(tec_xml)
# Adding one irrigation operation
SticsRFiles:::add_stics_nodes(tec_doc, "irrigation")

# Adding three irrigation operations
SticsRFiles:::add_stics_nodes(tec_doc, "irrigation", nodes_nb = 3)
context("Reaserching parameter bounds")


test_that("Reaserching parameter bounds", {
   expect_equal(length(get_param_bounds_xml(xml_file, "profhum", "min")),2)
   expect_equal(length(get_param_bounds_xml(xml_file, "profhum")),3)
   expect_equal(length(get_param_bounds_xml(c(xml_file,plt_file), c("profhum","codemonocot"))),2)
   expect_warning(length(get_param_bounds_xml(xml_file, "profhum")))
   expect_equal(length(get_param_bounds_xml(xml_file, "profhum",c("min", "max"))),3)
})


