library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version

context("Getting and Setting values from txt files")

path <- get_examples_path("txt", stics_version = stics_version)
# Copy example to test in tempdir since the files will be modified by set_param
file.copy(
  from = file.path(path, list.files(path)), to = tempdir(),
  overwrite = TRUE
)
path <- tempdir()

tmp1 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
codevar <- unlist(get_param_txt(
  workspace = path, param = "codevar",
  stics_version = stics_version
))
tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  variety = codevar, stics_version = stics_version
))
variete <- unlist(get_param_txt(
  workspace = path, param = "variete",
  stics_version = stics_version
))
tmp3 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  variety = variete, stics_version = stics_version
))

test_that("variety argument can take NULL, integer or characters", {
  expect_equal(tmp1, tmp2)
  expect_equal(tmp2, tmp3)
})

varieties <- get_param_txt(
  workspace = path,
  stics_version = stics_version
)$plant$plant1$codevar[1:3]

tmp1 <- get_param_txt(
  workspace = path, param = "stlevamf",
  variety = varieties,
  stics_version = stics_version
)$plant$plant1$stlevamf
tmp2 <- get_param_txt(
  workspace = path,
  stics_version = stics_version
)$plant$plant1$stlevamf[1:3]
test_that("variety argument can be a vector of characters", {
  expect_equal(tmp1, tmp2)
})

# Get and modify the non-varietal parameter "forme"
# (another parameter has a similar name : rapforme ...)

tmp <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
set_param_txt(
  workspace = path, param = "forme",
  value = tmp + 1,
  stics_version = stics_version
)

tmp2 <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
test_that("Set and get of a non-varietal parameter for a unique plant", {
  expect_equal(length(tmp), 1)
  expect_equal(tmp + 1, tmp2)
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
set_param_txt(
  workspace = path, param = "stlevamf",
  value = tmp + 1,
  stics_version = stics_version
)

tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
test_that("Set and get of a varietal parameter for a unique plant
          for the simulated variety", {
  expect_equal(tmp + 1, tmp2)
})

# Get and modify the varietal parameter "stlevamf" for a given variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "stlevamf", variety = 4,
  stics_version = stics_version
))
set_param_txt(
  workspace = path,
  param = "stlevamf",
  value = as.numeric(tmp) + 1,
  variety = 4,
  stics_version = stics_version
)

tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf", variety = 4,
  stics_version = stics_version
))
test_that("Set and get of a varietal parameter for a unique plant
          for a given variety", {
  expect_equal(tmp + 1, tmp2)
})


# Now let's work on intercrops ...
path <- file.path(
  get_examples_path("txt", stics_version = stics_version),
  "intercrop_pea_barley"
)
# Copy example to test in tempdir since the files will be modified by set_param
file.copy(
  from = file.path(path, list.files(path)), to = tempdir(),
  overwrite = TRUE
)
path <- tempdir()

# Get and modify the non-varietal parameter "forme" for the simulated variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
plant <- 2
set_param_txt(
  workspace = path,
  param = "forme",
  value = tmp[plant] + 1,
  plant_id = plant,
  stics_version = stics_version
)
tmp2 <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
test_that("Set and get of a non-varietal parameter for an intercrop
          for the simulated variety", {
  # expect_equal(as.numeric(tmp[plant]) + 1, as.numeric(tmp2[plant]))
  expect_equal(tmp[[plant]] + 1, tmp2[[plant]])
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
plant <- 2
set_param_txt(
  workspace = path, param = "stlevamf",
  value = tmp[[plant]] + 1,
  plant_id = 2,
  stics_version = stics_version
)
tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
test_that("Set and get of a varietal parameter for an intercrop
          for the simulated variety", {
  expect_equal(tmp[[plant]] + 1, tmp2[[plant]])
})
