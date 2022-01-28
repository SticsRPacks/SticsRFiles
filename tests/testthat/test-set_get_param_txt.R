library(SticsRFiles)

context("Getting and Setting values from txt files")

path_to_JavaStics=system.file("stics", package = "SticsRTests")
javastics_path=file.path(path_to_JavaStics,"V90")

data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", version_name = "V9.0"))
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path)
javastics_workspace_path<-file.path(data_dir,"XmlFiles")
SticsRFiles::gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
                              target_path = stics_inputs_path, verbose = TRUE,
                              usms_list="bo96iN+")
path <- file.path(stics_inputs_path,"bo96iN+")

tmp1 <- get_param_txt(dirpath = path, param = "stlevamf")
codevar <- get_param_txt(dirpath = path, param = "codevar")[[1]]
tmp2 <- get_param_txt(dirpath = path, param = "stlevamf", variety = codevar)
variete <- as.numeric(get_param_txt(dirpath = path, param = "variete")[[1]])
tmp3 <- get_param_txt(dirpath = path, param = "stlevamf", variety = variete)

test_that ("variety argument can take NULL, integer or characters", {
  expect_equal(tmp1,tmp2)
  expect_equal(tmp2,tmp3)
})

varieties <- get_param_txt(dirpath = path)$plant$plant1$codevar[1:3]
tmp1 <- as.numeric(get_param_txt(dirpath = path, param = "stlevamf", variety = varieties))
tmp2 <- get_param_txt(dirpath = path)$plant$plant1$stlevamf[1:3]
test_that ("variety argument can be a vector of characters", {
  expect_equal(tmp1,tmp2)
})

# Get and modify the non-varietal parameter "forme" (another parameter has a similar name : rapforme ...)
tmp <- get_param_txt(dirpath = path, param = "forme")
set_param_txt(dirpath = path, param = "forme",
              value=as.numeric(tmp)+1)
tmp2 <- get_param_txt(dirpath = path, param = "forme")
test_that ("Set and get of a non-varietal parameter for a unique plant", {
  expect_equal(length(tmp),1)
  expect_equal(as.numeric(tmp)+1,as.numeric(tmp2))
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- get_param_txt(dirpath = path, param = "stlevamf")
set_param_txt(dirpath = path, param = "stlevamf",
              value=as.numeric(tmp)+1)
tmp2 <- get_param_txt(dirpath = path, param = "stlevamf")
test_that ("Set and get of a varietal parameter for a unique plant for the simulated variety", {
  expect_equal(as.numeric(tmp)+1,as.numeric(tmp2))
})

# Get and modify the varietal parameter "stlevamf" for a given variety
tmp <- get_param_txt(dirpath = path, param = "stlevamf", variety=4)
set_param_txt(dirpath = path, param = "stlevamf",
              value=as.numeric(tmp)+1, variety=4)
tmp2 <- get_param_txt(dirpath = path, param = "stlevamf", variety=4)
test_that ("Set and get of a varietal parameter for a unique plant for a given variety", {
  expect_equal(as.numeric(tmp)+1,as.numeric(tmp2))
})


# Now let's work on intercrops ...
javastics_workspace_path = file.path(javastics_path,"example")
SticsRFiles::gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
                              target_path = stics_inputs_path, verbose = TRUE,
                              usms_list="intercrop_pea_barley")

# Get and modify the non-varietal parameter "forme" for the simulated variety
tmp <- get_param_txt(dirpath = file.path(stics_inputs_path,"intercrop_pea_barley"), param = "forme")
plant <- 2
set_param_txt(dirpath = file.path(stics_inputs_path,"intercrop_pea_barley"), param = "forme",
              value=as.numeric(tmp[plant])+1, plant=plant)
tmp2 <- get_param_txt(dirpath = file.path(stics_inputs_path,"intercrop_pea_barley"), param = "forme")
(as.numeric(tmp[plant])+1)==as.numeric(tmp2[plant])
test_that ("Set and get of a non-varietal parameter for an intercrop for the simulated variety", {
  expect_equal(as.numeric(tmp[plant])+1,as.numeric(tmp2[plant]))
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- get_param_txt(dirpath = file.path(stics_inputs_path,"intercrop_pea_barley"), param = "stlevamf")
plant <- 2
set_param_txt(dirpath = file.path(stics_inputs_path,"intercrop_pea_barley"), param = "stlevamf",
              value=as.numeric(tmp[plant])+1, plant=2)
tmp2 <- get_param_txt(dirpath = file.path(stics_inputs_path,"intercrop_pea_barley"), param = "stlevamf")
test_that ("Set and get of a varietal parameter for an intercrop for the simulated variety", {
  expect_equal(as.numeric(tmp[plant])+1,as.numeric(tmp2[plant]))
})

