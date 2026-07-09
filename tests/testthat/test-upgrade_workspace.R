# from 9 to 10
workspace <- file.path(
  download_data(stics_version = "V9.2", example_dirs = "study_case_1"),
  "XmlFiles"
)

out_dir <- file.path(workspace, "V10")
if (!dir.exists(out_dir)) dir.create(out_dir)

upgrade_workspace_xml(workspace = workspace, out_dir = out_dir, verbose = FALSE)

# from 10 to 11
out_dir <- file.path(workspace, "V11")
if (!dir.exists(out_dir)) dir.create(out_dir)
workspace <- file.path(workspace, "V10")
upgrade_workspace_xml(workspace = workspace, out_dir = out_dir, verbose = FALSE)
