context("Ckecking parameters in xml files and their values")

path <- get_examples_path("xml")

# Copy example to test in tempdir since the files will be modified by set_param
file.copy(
  from = file.path(path, list.files(path)),
  to = tempdir(),
  overwrite = TRUE
)

file <- file.path(tempdir(), "file_tec.xml")


test_that("Testing param types", {
  expect_equal(SticsRFiles:::get_xml_param_type(file, "iplt0"), "numeric")
  expect_equal(
    SticsRFiles:::get_xml_param_type(
      file,
      c("iplt0", "h2ograinmin", "ressuite")
    ),
    list("numeric", "numeric", "character")
  )

  expect_null(SticsRFiles:::get_xml_param_type(file, "a"))

  expect_equal(
    SticsRFiles:::get_xml_param_type(
      file,
      c("iplt0", "a", "ressuite")
    ),
    list("numeric", NULL, "character")
  )
})


test_that("Testing empty values", {
  expected_type <- SticsRFiles:::get_xml_param_type(file, "iplt0")
  expect_true(SticsRFiles:::is_empty_value(0, expected_type))
  expect_true(SticsRFiles:::is_empty_value(999, expected_type))
  expect_true(SticsRFiles:::is_empty_value(-999, expected_type))
  expect_true(SticsRFiles:::is_empty_value(as.numeric(NA), expected_type))
  expect_false(SticsRFiles:::is_empty_value(10, expected_type))

  expected_type <- SticsRFiles:::get_xml_param_type(file, "ressuite")
  expect_true(SticsRFiles:::is_empty_value("", expected_type))
  expect_true(SticsRFiles:::is_empty_value("999", expected_type))
  expect_true(SticsRFiles:::is_empty_value("-999", expected_type))
  expect_true(SticsRFiles:::is_empty_value(as.character(NA), expected_type))
  expect_false(SticsRFiles:::is_empty_value("ttt", expected_type))
})


test_that("Testing mandatory empty parameters", {
  set_param_xml(file = file, param = "iplt0", 0, overwrite = TRUE)
  expect_true(SticsRFiles:::check_mandatory_parameters(
    par_names = "iplt0",
    xml_file = file
  ))
  set_param_xml(file = file, param = "iplt0", 15, overwrite = TRUE)
  expect_false(SticsRFiles:::check_mandatory_parameters(
    par_names = "iplt0",
    xml_file = file
  ))
})
