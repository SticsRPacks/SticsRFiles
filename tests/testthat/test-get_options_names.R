stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

if (version_num < 11) {
  options_number <- 45
} else {
  options_number <- 46
}

context("Searching option names")

xml_path <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")

test_that("Reaserching option names", {
  expect_equal(length(get_options_names(xml_path)), options_number)
  expect_equal(
    length(get_options_names(xml_path, c("codemonocot", "codlainet"))),
    2
  )
})
