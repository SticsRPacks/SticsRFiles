library(SticsRFiles)
options(warn=-1)
xml_path= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.0/usms.xml"))
context("Getting usms param values")

subsidiary_path1 = xml_path
subsidiary_path2 = xml_path

test_that("getting all param from an usm", {
  expect_equal(as.character(unname(unlist(get_param_xml(xml_path,c("datedebut","datefin","finit","nomsol",
                                                      "fstation","fclim1","fclim2","culturean",
                                                      "nbplantes","codesimul"),
                                           parent_name = "usm",parent_sel_attr = "SugarCane")))),
               c(286,650,"canne_ini.xml","solcanne","climcanj_sta.xml","climcanj.1998",
                 "climcanj.1999",0,1,0))
})

