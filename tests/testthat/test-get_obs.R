context("reading observations")

path <- system.file(file.path("extdata","obs","V9.0"), package = "SticsRFiles")

# Get observations for all usms, but only banana has observations:
Meas <- get_obs(path)
# Get observations only for banana:
Meas_banana <- get_obs(path, "banana")



test_that("no observation return an empty data.frame", {
  expect_true(is.data.frame(Meas$SugarCane))
})

usms= get_usms_list(usm_path = file.path(path,"usms.xml"))

test_that("observation list length is equal to usms list length", {
  expect_equal(length(Meas), length(usms))
  expect_equal(names(Meas), usms)
})

test_that("observation list names are equal to usms names", {
  expect_equal(names(Meas), usms)
})

test_that("filtering observation list works", {
  expect_equal(length(Meas_banana), 1)
})


test_that("filtering observation list return a list of data.frame", {
  expect_true(is.list(Meas_banana)&!is.data.frame(Meas_banana))
  expect_true(is.data.frame(Meas_banana$banana))
})

