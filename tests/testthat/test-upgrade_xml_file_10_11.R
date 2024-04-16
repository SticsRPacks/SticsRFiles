
stics_from_version <- "V10.0"
stics_to_version <- get_stics_versions_compat()$latest_version

context("Upgrading xml files to latest version")

path_from <- get_examples_path("xml", stics_version = stics_from_version)

# Copy example to test in tempdir since the files will be modified by set_param
file.copy(
  from = file.path(path_from, list.files(path_from)), to = tempdir(),
  overwrite = TRUE
)
path <- tempdir()

out_path <- file.path(path, "out_xml")
if (!dir.exists(out_path)) dir.create(out_path)

upgrade_ini_xml_10_11(
  file = file.path(path, "file_ini.xml"),
  out_dir = out_path
)

upgrade_plt_xml_10_11(
  file = file.path(path, "file_plt.xml"),
  out_dir = out_path
)

upgrade_sta_xml_10_11(
  file = file.path(path, "file_sta.xml"),
  out_dir = out_path
)

upgrade_tec_xml_10_11(
  file = file.path(path, "file_tec.xml"),
  out_dir = out_path
)


upgrade_param_gen_xml_10_11(
  file = file.path(path, "param_gen.xml"),
  out_dir = out_path
)

upgrade_param_newform_xml_10_11(
  file = file.path(path, "param_newform.xml"),
  out_dir = out_path
)

upgrade_sols_xml_10_11(
  file = file.path(path, "sols.xml"),
  out_dir = out_path,
)

upgrade_usms_xml_10_11(
  file = file.path(path, "usms.xml"),
  out_dir = out_path
)


target_path <- get_examples_path("xml", stics_version = stics_to_version)
for (file_path in list.files(out_path, full.names = TRUE)) {
  file_name <- basename(file_path)
  # loading files
  out_xml <- xmldocument(file_path)
  target_xml <- xmldocument(file.path(target_path, file_name))

  test_that(paste(file_name, "XML documents match"), {
    expect_equal(out_xml@content, target_xml@content)
  })
}
