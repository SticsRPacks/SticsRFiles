# Generate a sols.xml file

library(SticsOnR)
library(readxl)
library(SticsRFiles)


out_file <- "/path/to/file/sols.xml" # or something like C://path//to//file//sols.xml" for Windows
xl_dir <- "/path/to/xl/dir"          # or something like C://path//to//xl//dir" for Windows

# Using an XL example file (from the package)
copy_mailing_example("inputs_stics_example.xlsx", dest_dir = xl_dir)
xl_path <- file.path(xl_dir, "inputs_stics_example.xlsx")

# Reading the xl file
xl_param <- read_excel(xl_path,sheet = "Soils")

# Generating a new sols.xml file, for all xl_param lines
gen_sols_xml(sols_param = xl_param, sols_out_file = out_file)
