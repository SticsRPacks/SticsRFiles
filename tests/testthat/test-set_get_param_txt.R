library(SticsRFiles)

context("Getting and Setting values from txt files")

path <- get_examples_path("txt")
# Copy example to test in tempdir since the files will be modified by set_param
file.copy(from=file.path(path,list.files(path)), to=tempdir(), overwrite = TRUE)
path <- tempdir()

tmp1 <- get_param_txt(workspace = path, param = "stlevamf")
codevar <- get_param_txt(workspace = path, param = "codevar")[[1]]
tmp2 <- get_param_txt(workspace = path, param = "stlevamf", variety = codevar)
variete <- as.numeric(get_param_txt(workspace = path, param = "variete")[[1]])
tmp3 <- get_param_txt(workspace = path, param = "stlevamf", variety = variete)

test_that ("variety argument can take NULL, integer or characters", {
  expect_equal(tmp1,tmp2)
  expect_equal(tmp2,tmp3)
})

varieties <- get_param_txt(workspace = path)$plant$plant1$codevar[1:3]
tmp1 <- as.numeric(get_param_txt(workspace = path, param = "stlevamf", variety = varieties))
tmp2 <- get_param_txt(workspace = path)$plant$plant1$stlevamf[1:3]
test_that ("variety argument can be a vector of characters", {
  expect_equal(tmp1,tmp2)
})

# Get and modify the non-varietal parameter "forme" (another parameter has a similar name : rapforme ...)
tmp <- get_param_txt(workspace = path, param = "forme", exact=TRUE)
set_param_txt(workspace = path, param = "forme",
              value=as.numeric(tmp)+1)
tmp2 <- get_param_txt(workspace = path, param = "forme", exact=TRUE)
test_that ("Set and get of a non-varietal parameter for a unique plant", {
  expect_equal(length(tmp),1)
  expect_equal(as.numeric(tmp)+1,as.numeric(tmp2))
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- get_param_txt(workspace = path, param = "stlevamf")
set_param_txt(workspace = path, param = "stlevamf",
              value=as.numeric(tmp)+1)
tmp2 <- get_param_txt(workspace = path, param = "stlevamf")
test_that ("Set and get of a varietal parameter for a unique plant for the simulated variety", {
  expect_equal(as.numeric(tmp)+1,as.numeric(tmp2))
})

# Get and modify the varietal parameter "stlevamf" for a given variety
tmp <- get_param_txt(workspace = path, param = "stlevamf", variety=4)
set_param_txt(workspace = path, param = "stlevamf",
              value=as.numeric(tmp)+1, variety=4)
tmp2 <- get_param_txt(workspace = path, param = "stlevamf", variety=4)
test_that ("Set and get of a varietal parameter for a unique plant for a given variety", {
  expect_equal(as.numeric(tmp)+1,as.numeric(tmp2))
})


# Now let's work on intercrops ...
path <- file.path(get_examples_path("txt"),"intercrop_pea_barley")
# Copy example to test in tempdir since the files will be modified by set_param
file.copy(from=file.path(path,list.files(path)), to=tempdir(), overwrite = TRUE)
path <- tempdir()

# Get and modify the non-varietal parameter "forme" for the simulated variety
tmp <- get_param_txt(workspace = path, param = "forme", exact=TRUE)
plant <- 2
set_param_txt(workspace = path, param = "forme",
              value=as.numeric(tmp[plant])+1, plant_id=plant)
tmp2 <- get_param_txt(workspace = path, param = "forme", exact=TRUE)
test_that ("Set and get of a non-varietal parameter for an intercrop for the simulated variety", {
  expect_equal(as.numeric(tmp[plant])+1,as.numeric(tmp2[plant]))
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- get_param_txt(workspace = path, param = "stlevamf")
plant <- 2
set_param_txt(workspace = path, param = "stlevamf",
              value=as.numeric(tmp[plant])+1, plant_id=2)
tmp2 <- get_param_txt(workspace = path, param = "stlevamf")
test_that ("Set and get of a varietal parameter for an intercrop for the simulated variety", {
  expect_equal(as.numeric(tmp[plant])+1,as.numeric(tmp2[plant]))
})

