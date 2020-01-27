library(SticsRFiles)
options(warn=-1)
xml_path= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.1/file_ini.xml"))
context("Getting initialisation param values")

test_that("single param option value ", {
  expect_equal(unlist(get_param_xml(xml_path,"nbplantes")),1)
})


# unlist with different types produces a character vector
test_that("multiple param option value", {
  val <- unlist(get_param_xml(xml_path,c("stade0","lai0","masec0","QNplante0",
                                  "magrain0","zrac0","resperenne0"),
                       select ="plante",value = 1 ), use.names = FALSE)

  expect_equal(val, c("snu","0","0","0","0","0","0"))
})


test_that("multiple param option value 2", {
  val <- unlist(get_param_xml(xml_path,"densinitial", select ="plante",
                                     value = 1 ))
  expect_equal(val, c(0,0,0,0,0))
})

test_that("multiple values from single node", {
  val <- unlist(get_param_xml(xml_path,"hinit"))
  expect_equal(val, c(23.5,21.6,23.9,27.6,0))
})

# test_that("multiple values from single node with parent node specified", {
#   expect_equal(unlist(get_param_xml(xml_path,"hinit",select = "sol")),
#                c(23.5,21.6,23.9,27.6,0))
# })

test_that("multiple values from multiple nodes", {
  val <- unlist(get_param_xml(xml_path,c("hinit","NO3init","NH4init")), use.names = FALSE)
  expect_equal(val,c(c(23.5,21.6,23.9,27.6,0),c(32,12,9,0,0),c(0,0,0,0,0)))
})


