context("reading observations")

path <- file.path(get_examples_path(file_type = "obs"),"simple_exemple")

# Get observations for all usms, but only banana has observations:
Meas <- get_obs(path)

# Get observations only for banana:
Meas_banana <- get_obs(path, "banana")

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
# test_that("filtering observation list works", {
#   expect_equal(length(Meas_banana), 1)
# })
#
#
test_that("filtering observation list return a list of data.frame", {
  expect_true(is.list(Meas_banana)&!is.data.frame(Meas_banana))
  expect_true(is.data.frame(Meas_banana$banana))
})

