# generate an usms.xml file

library(SticsOnR)
library(readxl)
library(Classes)

out_file <- "/path/to/file/usms.xml" # or something like C://path//to//file//usms.xml" for Windows
xl_dir <- "/path/to/xl/dir"          # or something like C://path//to//xl//dir" for Windows

# Using an XL example file (from the package)
get_inputs_example("inputs_stics_example.xlsx", dest_dir = xl_dir)
xl_path <- file.path(xl_dir, "inputs_stics_example.xlsx")

# Reading the xl file
xl_param <- read_excel(xl,sheet = "USMs")

# Generating a new usms.xml file, for all xl_param lines
gen_usms_file(usms_param = xl_param, usms_out_file = out_file)
