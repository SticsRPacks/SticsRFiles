# Getting data sets for upgrades and comparison with target files
# for each version
workspace_9 <- file.path(
  download_data(stics_version = "V9.2", example_dirs = "study_case_1"),
  "XmlFiles"
)

workspace_10 <- file.path(
  download_data(stics_version = "V10", example_dirs = "study_case_1"),
  "XmlFiles"
)

workspace_11 <- file.path(
  download_data(stics_version = "V11", example_dirs = "study_case_1"),
  "XmlFiles"
)

# from 9 to 10

out_dir <- file.path(workspace_9, "V10")
if (!dir.exists(out_dir)) dir.create(out_dir)

upgrade_workspace_xml(
  workspace = workspace_9,
  out_dir = out_dir,
  verbose = FALSE
)
# checking that every upgraded file matches target version files
for (file_path in list.files(pattern = "\\.xml", out_dir, full.names = TRUE)) {
  file_name <- basename(file_path)
  # loading files
  out_xml <- SticsRFiles:::xmldocument(file_path)
  target_xml <- SticsRFiles:::xmldocument(file.path(workspace_10, file_name))

  test_that(paste(file_name, "XML documents match"), {
    expect_equal(out_xml@content, target_xml@content)
  })
}


# from 10 to 11
out_dir <- file.path(workspace_9, "V11")
if (!dir.exists(out_dir)) dir.create(out_dir)
workspace <- file.path(workspace_9, "V10")
upgrade_workspace_xml(workspace = workspace, out_dir = out_dir, verbose = FALSE)

# checking that every upgraded file matches target version files
for (file_path in list.files(pattern = "\\.xml", out_dir, full.names = TRUE)) {
  file_name <- basename(file_path)
  # loading files
  out_xml <- SticsRFiles:::xmldocument(file_path)
  target_xml <- SticsRFiles:::xmldocument(file.path(workspace_10, file_name))

  test_that(paste(file_name, "XML documents match"), {
    expect_equal(out_xml@content, target_xml@content)
  })
}
