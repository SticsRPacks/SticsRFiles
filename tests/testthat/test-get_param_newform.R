library(SticsRFiles)
options(warn=-1)
xml_path= file.path(get_examples_path("xml"), "param_newform.xml")
context("Getting newform param values")

test_that("get option parameters from option node", {
  expect_equal(unname(unlist(get_param_xml(xml_path,c("SurfApex(1)","SeuilMorTalle(1)","SigmaDisTalle(1)",
                                                      "VitReconsPeupl(1)","SeuilReconsPeupl(1)",
                                                      "MaxTalle(1)","SeuilLAIapex(1)",
                                                      "tigefeuilcoupe(1)")))),
               c(5e-6,0.02,0.1,1e-5,800,4000,1,5))
})
