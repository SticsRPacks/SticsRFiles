stics_version <- get_stics_versions_compat()$latest_version

xml_plt <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")
xsl_file <- file.path(get_examples_path(file_type = "xsl"), "xml2txt.xsl")
out_dir <- file.path(tempdir(), "Test_Convert")
if (!dir.exists(out_dir)) dir.create(out_dir)
convert_xml2txt_int(
  xml_file = xml_plt,
  style_file = xsl_file,
  file.path(out_dir, "ficplt.txt")
)

context("Convert file type")

test_that("Convert xml file to txt file", {
  expect_true(file.exists(file.path(out_dir, "ficplt.txt")))
})


context("convert all files of a workspace")
root_dir <- download_data(example_dirs = "study_case_1")
workspace <- file.path(root_dir, "XmlFiles")
gen_usms_xml2txt(
  workspace = workspace
)

usms_list <- get_usms_list(file.path(workspace, "usms.xml"))
sub_dirs <- list.dirs(workspace, full.names = FALSE)[-c(1, 10)]
test_that("Convert a workspace xml to txt file", {
  expect_true(all(file.exists(file.path(
    workspace,
    rep(sub_dirs, 11),
    c(
      "climat.txt",
      "param.sol",
      "ficini.txt",
      "ficplt1.txt",
      "fictec1.txt",
      "station.txt",
      "new_travail.usm",
      "tempopar.sti",
      "tempoparv6.sti"
    )
  ))))

  expect_true(all(file.exists(file.path(
    workspace,
    rep(sub_dirs, 3),
    c("var.mod", "rap.mod", "prof.mod")
  ))))
})
