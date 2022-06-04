library(SticsRFiles)
# options(warn=-1)
xml_path <- file.path(get_examples_path("xml"), "param_gen.xml")
context("Getting gen param values")

test_that("all values from a single node", {
  expect_equal(
    unname(unlist(get_param_xml(xml_path, c("masvolcx", "hcccx"),
      select = "Lutetian Brackish marl and limestone"
    ))),
    c(2.3, 5)
  )
})

test_that("getting values from choice param", {
  expect_equal(
    unname(unlist(get_param_xml(xml_path, c("tnitmin", "tnitopt", "tnitopt2", "tnitmax")))),
    c(5, 30, 35, 58)
  )
})

