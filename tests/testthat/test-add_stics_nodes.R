library(SticsRFiles)

stics_version <- "V9.0"
#get_stics_versions_compat()$latest_version
tec_xml <- file.path(get_examples_path(file_type = "xml"),stics_version, "file_tec.xml")
tec_doc <- SticsRFiles:::xmldocument(tec_xml)


context("Getting and Setting values from txt files")
#SticsRFiles:::add_stics_nodes(tec_doc, "irrigation", nodes_nb = 3)
test_that("variety argument can take NULL, integer or characters", {
  expect_error (SticsRFiles:::add_stics_nodes(tec_doc, "irrigation", nodes_nb = 3))
  expect_error (SticsRFiles:::add_stics_nodes(tec_xml, "irrigation", nodes_nb = 3))
  #expect_message (SticsRFiles:::add_stics_nodes(tec_doc, "irrigation", nodes_nb = 3),"The xml document is of wrong type")
  #expect_equal(tmp2, tmp3)
})
