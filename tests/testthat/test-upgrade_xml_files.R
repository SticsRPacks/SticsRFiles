# first test from 10 to 11------------------------------------------------------

starting_version <- "V10.0"
target_version <- "V11.0"

context(paste("Upgrading xml files to", target_version))

path_from <- get_examples_path("xml", stics_version = starting_version)

# Copy example to test in tempdir since the files will be modified by set_param
path <- file.path(tempdir(), paste0("upgrade_to_", target_version))
if (!dir.exists(path)) dir.create(path)
file.copy(
  from = file.path(path_from, list.files(path_from)),
  to = file.path(path, list.files(path_from)),
  overwrite = TRUE
)


out_path <- file.path(path, paste0("out_xml_", target_version))
if (!dir.exists(out_path)) dir.create(out_path)

#
par_gen <- file.path(path, "param_gen.xml")
par_new <- file.path(path, "param_newform.xml")

upgrade_ini_xml(
  file = file.path(path, "file_ini.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_plt_xml(
  file = file.path(path, "file_plt.xml"),
  out_dir = out_path,
  param_gen_file = par_gen,
  param_newform_file = par_new,
  warning = FALSE
)

upgrade_sta_xml(
  file = file.path(path, "file_sta.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_tec_xml(
  file = file.path(path, "file_tec.xml"),
  out_dir = out_path,
  param_gen_file = par_gen,
  param_newform_file = par_new
)


upgrade_param_gen_xml(
  file = par_gen,
  out_dir = out_path
)

upgrade_param_newform_xml(
  file = par_new,
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_sols_xml(
  file = file.path(path, "sols.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_usms_xml(
  file = file.path(path, "usms.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

target_path <- get_examples_path("xml", stics_version = target_version)
for (file_path in list.files(out_path, full.names = TRUE)) {
  file_name <- basename(file_path)
  # loading files
  out_xml <- xmldocument(file_path)
  target_xml <- xmldocument(file.path(target_path, file_name))

  test_that(paste(file_name, "XML documents match"), {
    expect_equal(out_xml@content, target_xml@content)
  })
}

# second test from 9 to 10------------------------------------------------------
starting_version <- "V9.2"
target_version <- "V10.0"

context(paste("Upgrading xml files to", target_version))

path_from <- get_examples_path("xml", stics_version = starting_version)

# Copy example to test in tempdir since the files will be modified by set_param
path <- file.path(tempdir(), paste0("upgrade_to_", target_version))
if (!dir.exists(path)) dir.create(path)
file.copy(
  from = file.path(path_from, list.files(path_from)),
  to = file.path(path, list.files(path_from)),
  overwrite = TRUE
)


out_path <- file.path(path, paste0("out_xml", target_version))
if (!dir.exists(out_path)) dir.create(out_path)

par_gen <- file.path(path, "param_gen.xml")
par_new <- file.path(path, "param_newform.xml")

upgrade_ini_xml(
  file = file.path(path, "file_ini.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_plt_xml(
  file = file.path(path, "file_plt.xml"),
  out_dir = out_path,
  param_gen_file = par_gen,
  param_newform_file = par_new,
  warning = FALSE
)

upgrade_sta_xml(
  file = file.path(path, "file_sta.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_tec_xml(
  file = file.path(path, "file_tec.xml"),
  out_dir = out_path,
  param_gen_file = par_gen,
  param_newform_file = par_new
)


upgrade_param_gen_xml(
  file = par_gen,
  out_dir = out_path
)

upgrade_param_newform_xml(
  file = par_new,
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_sols_xml(
  file = file.path(path, "sols.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

upgrade_usms_xml(
  file = file.path(path, "usms.xml"),
  out_dir = out_path,
  param_gen_file = par_gen
)

target_path <- get_examples_path("xml", stics_version = target_version)
for (file_path in list.files(out_path, full.names = TRUE)) {
  file_name <- basename(file_path)
  # loading files
  out_xml <- xmldocument(file_path)
  target_xml <- xmldocument(file.path(target_path, file_name))

  test_that(paste(file_name, "XML documents match"), {
    expect_equal(out_xml@content, target_xml@content)
  })
}
