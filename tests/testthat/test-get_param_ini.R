library(SticsRFiles)
options(warn=-1)
xml_path= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.0/file_ini.xml"))
context("Getting initialisation param values")

test_that("single param option value ", {
  expect_equal(unlist(get_param_xml(xml_path,"nbplantes")),1)
})


# unname(query) because result is just numbers, it doesn't have names
# as.character() because in this case we have differents types in the response, numeric and character,
#   and the output vector is cast in a character vector
test_that("multiple param option value", {
  expect_equal(as.character(unlist(unname(get_param_xml(xml_path,c("stade0","lai0","masec0","QNplante0",
                                                      "magrain0","zrac0","resperenne0"),
                                           parent_name ="plante",parent_sel_attr = 1 )))),
               c("snu",0,0,0,0,0,0))
})


# this test's output is good but not in the expected format
# expected format : numeric vector c(0,0,0,0,0)
# output format : character vector made from concatenated values ("0.00.00.00.00.0")
test_that("multiple param option value 2", {
  expect_equal(unlist(unname(get_param_xml(xml_path,"densinitial", parent_name ="plante",
                                           parent_sel_attr = 1 ))),c(0,0,0,0,0))
})

# same as precedent
test_that("multiple values from single node", {
  expect_equal(unlist(get_param_xml(xml_path,"hinit")),c(23.5,21.6,23.9,27.6,0))
})

#same as precedent
test_that("multiple values from single node with parent node specified", {
  expect_equal(unlist(get_param_xml(xml_path,"hinit",parent_name = "sol")),c(23.5,21.6,23.9,27.6,0))
})

# same as precedents
test_that("multiple values from multiple nodes", {
  expect_equal(unlist(unname(get_param_xml(xml_path,c("hinit","NO3init","NH4init")))),c(
              c(23.5,21.6,23.9,27.6,0),c(32,12,9,0,0),c(0,0,0,0,0)))
})

# => get_param_xml returns a character vector for xml when nodes are not in table
