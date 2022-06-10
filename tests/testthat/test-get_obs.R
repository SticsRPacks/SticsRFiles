#stics_version <- "V9.2"
stics_version <- get_stics_versions_compat()$latest_version


context("reading observations")

path <- file.path(get_examples_path(file_type = "obs", stics_version = stics_version), "simple_example")

path_mixed <- file.path(get_examples_path(file_type = "obs"), "mixed")

# Get observations for all usms, but only banana has observations:
Meas <- get_obs(workspace = path)
Meas_mixed <- get_obs(path_mixed)

# Get observations only for banana:
Meas_banana <- get_obs(path_mixed, "banana")

# Get more information using the usms.xml file:

Meas_mixed2 <- get_obs(workspace = path_mixed, usms_file = file.path(path_mixed, "usms.xml"))

# Using a usms.xml from outside the repo:
usms_path <- file.path(get_examples_path(file_type = "xml", stics_version = stics_version), "usms.xml")
usms <- get_usms_list(file = usms_path)

Meas_mixed3 <- get_obs(workspace = path_mixed, usms_file = usms_path)

# This is not true anymore, it returns NULL because of the case were we have the
# usms.xml file outside of the folder and it references other usms too.
# test_that("no observation return an empty data.frame", {
#   expect_true(is.data.frame(Meas$SugarCane))
# })

# usms_path <- file.path(get_examples_path( file_type = "xml"),"usms.xml")
# usms= get_usms_list(file = usms_path)
#
# test_that("observation list length is equal to usms list length", {
#   expect_equal(length(Meas), length(usms))
#   expect_equal(names(Meas), usms)
# })
#
# test_that("observation list names are equal to usms names", {
#   expect_equal(names(Meas), usms)
# })
#

test_that("filtering observation list works", {
  expect_equal(length(Meas_banana), 1)
})

test_that("filtering observation list return a list of data.frame", {
  expect_true(is.list(Meas_banana) & !is.data.frame(Meas_banana))
  expect_true(is.data.frame(Meas_banana$banana))
})

test_that("reading mixed usms works", {
  expect_length(Meas_mixed, 3)
  expect_named(Meas_mixed, c("sorghum", "banana", "IC_banana_sorghum"), ignore.order = TRUE)
  expect_equal(sum(Meas_mixed$sorghum$lai_n, na.rm = TRUE), 19.62)
  expect_equal(sum(Meas_mixed$banana$lai_n, na.rm = TRUE), 34.446)
  expect_equal(sum(Meas_mixed$IC_banana_sorghum$lai_n, na.rm = TRUE), 54.066)
})

test_that("reading mixed usms with usms_filename to usms.xml", {
  expect_length(Meas_mixed2, 3)
  expect_equal(sort(names(Meas_mixed)), sort(names(Meas_mixed2)))
  expect_equal(Meas_mixed$IC_banana_sorghum$lai_n, Meas_mixed2$IC_banana_sorghum$lai_n)
})

test_that("reading mixed usms with usms_filename to usms.xml outside of folder", {
  expect_length(Meas_mixed3, 2) # NB: only two because intecrop usms absent from this usms.xml
  expect_equal(Meas_mixed$banana$lai_n, Meas_mixed3$banana$lai_n)
})

# Testing with .obs in different folders (e.g. one for each usm, but a common usms.xml)

test_that("reading mixed usms with usms_filename to usms.xml outside of folder, and usms in different folders", {
  path <- file.path(get_examples_path(file_type = "obs"), "usms_outside")
  paths <- list.dirs(path)[-1]
  Meas_2 <- get_obs(workspace = paths, usms_file = file.path(path, "usms.xml"))

  expect_length(Meas_mixed3, 2) # NB: only two because intecrop usms absent from this usms.xml
  expect_equal(Meas_mixed$banana$lai_n, Meas_mixed3$banana$lai_n)
})


# Testing empty obs:
test_that("reading empty usms returns a 0 row data", {
  path_empty <- file.path(get_examples_path(file_type = "obs"), "empty")
  meas <- get_obs(workspace = path_empty)
  expect_true(is.data.frame(meas$empty))
  expect_length(meas$empty, 0)
})


example_IC <- download_data(example_dirs = "study_case_intercrop", stics_version = stics_version)

test_that("get obs with intercrops", {
  outputs <- get_obs(workspace = example_IC)
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("plant_1", "plant_2")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
  expect_equal(unique(outputs$`SC_Pea_2005-2006_N0`$Plant), "plant_1")
  expect_null(outputs$`SC_Pea_2005-2006_N0`$Dominance)
})

test_that("get obs with intercrops, giving usms.xml file", {
  outputs <- get_obs(workspace = example_IC, usms_file = file.path(example_IC, "usms.xml"))
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("ble", "poi")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
})

test_that("get obs with intercrops, giving usms.xml file as absolute path", {
  outputs <- get_obs(workspace = example_IC, usms_file = file.path(example_IC, "usms.xml"))
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("ble", "poi")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
})

