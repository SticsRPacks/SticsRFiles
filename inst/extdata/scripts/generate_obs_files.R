# Generate observations files

library(readxl)
library(SticsRFiles)



out_dir <- "/path/to/dir"    # or something like C:/path/to/dir" for Windows
xl_dir <- "/path/to/xl/dir"   # or something like C:/path/to/xl/dir" for Windows

# Using an XL example file (from the package)
copy_mailing_example("inputs_stics_example.xlsx", dest_dir = xl_dir)
xl_path <- file.path(xl_dir, "inputs_stics_example.xlsx")

# Reading the xl file
obs_table <- read_excel(xl_path, sheet = "Obs")


## Generating a list of files from the obs_table table
# one for each usm, using lines for several observation dates

gen_usms_obs(obs_table, out_dir)
