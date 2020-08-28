context("reading observations")

path <- file.path(get_examples_path(file_type = "obs"),"simple_exemple")
path_mixed = file.path(get_examples_path(file_type = "obs"),"mixed")

# Get observations for all usms, but only banana has observations:
Meas <- get_obs(workspace = path)
Meas_mixed = get_obs(path_mixed)

# Get observations only for banana:
Meas_banana <- get_obs(path_mixed, "banana")

# Get more information using the usms.xml file:

Meas_mixed2 <- get_obs(workspace = path_mixed, usms_filename = "usms.xml")

# Using a usms.xml from outside the repo:
usms_path <- file.path(get_examples_path( file_type = "xml"),"usms.xml")
usms = get_usms_list(usm_path = usms_path)

Meas_mixed3 <- get_obs(workspace = path_mixed, usms_filename = usms_path)


# This is not true anymore, it returns NULL because of the case were we have the
# usms.xml file outside of the folder and it references other usms too.
# test_that("no observation return an empty data.frame", {
#   expect_true(is.data.frame(Meas$SugarCane))
# })

# usms_path <- file.path(get_examples_path( file_type = "xml"),"usms.xml")
# usms= get_usms_list(usm_path = usms_path)
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
  expect_true(is.list(Meas_banana)&!is.data.frame(Meas_banana))
  expect_true(is.data.frame(Meas_banana$banana))
})


test_that("reading mixed usms works", {
  expect_length(Meas_mixed,3)
  expect_equal(names(Meas_mixed),c("sorghum","banana","IC_banana_sorghum"))
  expect_known_hash(Meas_mixed, "e694fbccd2")
})

test_that("reading mixed usms works", {
  expect_length(Meas_mixed,3)
  expect_equal(names(Meas_mixed),c("sorghum","banana","IC_banana_sorghum"))
  expect_known_hash(Meas_mixed, "e694fbccd2")
})

test_that("reading mixed usms with usms_filename to usms.xml", {
  expect_length(Meas_mixed2,3)
  expect_equal(sort(names(Meas_mixed)),sort(names(Meas_mixed2)))
  expect_equal(Meas_mixed$IC_banana_sorghum$lai_n, Meas_mixed2$IC_banana_sorghum$lai_n)
})

test_that("reading mixed usms with usms_filename to usms.xml outside of folder", {
  expect_length(Meas_mixed3,2) # NB: only two because intecrop usms absent from this usms.xml
  expect_equal(Meas_mixed$banana$lai_n, Meas_mixed3$banana$lai_n)
})

# Testing with .obs in different folders (e.g. one for each usm, but a common usms.xml)

test_that("reading mixed usms with usms_filename to usms.xml outside of folder, and usms in different folders", {
  path = file.path(get_examples_path(file_type = "obs"),"usms_outside")
  paths = list.dirs(path)[-1]
  Meas_2 <- get_obs(workspace = paths, usms_filename = file.path(path,"usms.xml"))

  expect_length(Meas_mixed3,2) # NB: only two because intecrop usms absent from this usms.xml
  expect_equal(Meas_mixed$banana$lai_n, Meas_mixed3$banana$lai_n)
})



