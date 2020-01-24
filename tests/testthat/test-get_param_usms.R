library(SticsRFiles)
options(warn=-1)
xml_path= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.1/usms.xml"))
context("Getting usms param values")

test_that("getting all param from an usm", {
  expect_equal(as.character(unname(unlist(get_param_xml(xml_path,c("datedebut","datefin","finit","nomsol",
                                                      "fstation","fclim1","fclim2","culturean",
                                                      "nbplantes","codesimul"),
                                           parent_name = "usm",parent_sel_attr = "SugarCane")))),
               c(286,650,"canne_ini.xml","solcanne","climcanj_sta.xml","climcanj.1998",
                 "climcanj.1999",0,1,0))
  expect_equal(unname(unlist(get_param_xml(xml_path,"fplt",parent_name = "plante")[[1]][1])),
               "proto_sugarcane_plt.xml")
  expect_equal(unname(unlist(get_param_xml(xml_path,"ftec",parent_name = "plante")[[1]][1])),
               "canne_tec.xml")
  expect_equal(unname(unlist(get_param_xml(xml_path,"flai",parent_name = "plante")[[1]][1])),
               "null")

  expect_equal(as.character(unname(unlist(get_param_xml(xml_path,c("datedebut","datefin","finit","nomsol",
                                                                   "fstation","fclim1","fclim2","culturean",
                                                                   "nbplantes","codesimul"),
                                                        parent_name = "usm",parent_sel_attr = "potato")))),
               c(91,250,"patate_ini.xml","solpatate","climpdtj_sta.xml","climpdtj.1997",
                 "climpdtj.1997",1,1,0))
  expect_equal(unname(unlist(get_param_xml(xml_path,"fplt",parent_name = "plante")[[1]][3])),
               "proto_potato_plt.xml")
  expect_equal(unname(unlist(get_param_xml(xml_path,"ftec",parent_name = "plante")[[1]][3])),
               "patate_tec.xml")
  expect_equal(unname(unlist(get_param_xml(xml_path,"flai",parent_name = "plante")[[1]][3])),
               "null")
})

