stics_version <- get_stics_versions_compat()$latest_version

xml_plt <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")
out_dir <- file.path(tempdir(), "Test_Convert")
if (!dir.exists(out_dir)) dir.create(out_dir)
convert_xml2txt(file = xml_plt,out_dir = out_dir)
txt_plt <- file.path(get_examples_path(file_type = "txt"), "ficplt1.txt")


test_that("Convert xml file to txt file", {
  expect_true(file.exists(file.path(out_dir, "ficplt1.txt")))
#  expect_equal(readLines(file.path(out_dir, "ficplt1.txt")),
 #              readLines(txt_plt))
})
