library(SticsRFiles)
library(dplyr)
options(warn=-1)
xl_path= file.path(get_examples_path("xl"), "inputs_stics_example.xlsx")

ini_param <- read_params_table(file_path = xl_path, sheet_name = "Ini")

out_dir <- file.path(tempdir(), "gen_xml")
if(!dir.exists(out_dir)) dir.create(out_dir)

gen_ini_xml(param_table = ini_param[1, ], out_path = out_dir)

ini_xml <- file.path(out_dir, ini_param[1,]$Ini_name)


# For plante 1
xml_plt1_values <- unlist(get_param_xml(xml_file = ini_xml,select = "plante", value = 1)[[1]])

xl_plt1_values <- select(ini_param[1,], ends_with("Crop1"))

# For plant 2
xml_plt2_values <- unlist(get_param_xml(xml_file = ini_xml,select = "plante", value = 2)[[1]])

xl_plt2_values <- select(ini_param[1,], ends_with("Crop2"))

# for sol
# hinit

xml_hinit_values <- unlist(get_param_xml(xml_file = ini_xml, param_name = "hinit")[[1]])

xl_hinit_values <- select(ini_param[1,], starts_with("hinit"))

# NO3init
xml_NO3init_values <- unlist(get_param_xml(xml_file = ini_xml, param_name = "NO3init")[[1]])

xl_NO3init_values <- select(ini_param[1,], starts_with("NO3init"))

# NH4init
xml_NH4init_values <- unlist(get_param_xml(xml_file = ini_xml, param_name = "NH4init")[[1]])

xl_NH4init_values <- select(ini_param[1,], starts_with("NH4init"))

context("Comparing table values vs xml ini file values")


test_that ("Testing values for plant parameters", {
  expect_true(all(xml_plt1_values == xl_plt1_values))
  expect_equal(all(xml_plt2_values ==""), ncol(xl_plt2_values)==0)
})

test_that ("Testing values for soil parameters", {
  expect_true(all(xml_hinit_values == xl_hinit_values))
  expect_true(all(xml_NO3init_values == xl_NO3init_values))
  expect_true(all(xml_NH4init_values == xl_NH4init_values))
})
