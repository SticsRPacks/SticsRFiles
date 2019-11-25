# Generate sta files

library(SticsOnR)
library(readxl)
library(SticsRFiles)

out_dir <- "/path/to/dir/"     # or something like C://path//to//dir" for Windows
xl_dir <- "/path/to/xl/dir"    # or something like C://path//to//xl//dir" for Windows

# Using an XL example file (from the package)
get_inputs_example("inputs_stics_example.xlsx", dest_dir = xl_dir)
xl_path <- file.path(xl_dir, "inputs_stics_example.xlsx")

# Reading the xl file
xl_param <- read_excel(xl_path, sheet = "Station")

## Generating a list of files from the xl_param table
# for all the lines (one file per xl_param line)
# without using an xml input file (getting xml file from a template example in the package)
gen_sta_file(param_table = xl_param, out_path = out_dir)

# using an existing input xml file
in_file <- "/path/to/file_sta.xml"

gen_ini_file(sta_in_file = in_file ,param_table = xl_param, out_path = out_dir)
