library(SticsRFiles)
options(warn=-1)
xml_path= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.1/file_sta.xml"))
context("Getting station param values")

test_that("single value from single node" , {
  expect_equal(unlist(get_param_xml(xml_path,"codeclichange")),1)
})


test_that("singles values from multiples nodes",{
  expect_equal(unname(unlist(get_param_xml(xml_path,c("codeetp","alphapt","codeclichange","codaltitude",
                                               "altistation","altisimul","gradtn","gradtx",
                                               "altinversion","gradtninv","cielclair","codadret",
                                               "ombragetx")))),c(1,1.26,1,1,440,800,-0.5,-0.55,
                                                                500,1.3,0.8,1,-1.4))
})
