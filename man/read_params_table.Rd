% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_params_table.R
\name{read_params_table}
\alias{read_params_table}
\title{Getting parameters data from tables files (Excel sheet, csv)}
\usage{
read_params_table(file, sheet_name = NULL, num_na = "NA", char_na = "NA")
}
\arguments{
\item{file}{Excel or csv file path (including name of the file)}

\item{sheet_name}{Name of an Excel sheet (useless for csv files)}

\item{num_na}{Replacement value for numerical NA values (default: NA)}

\item{char_na}{Replacement value for character NA values (default: "")}
}
\value{
A tibble of parameters
}
\description{
Getting parameters data from tables files (Excel sheet, csv)
}
\details{
After data are loaded, numerical and string NA values are
replaced respectively with num_na or char_na
}
\examples{

usm_xl_file <- download_usm_xl(
  file = "inputs_stics_example.xlsx",
  verbose = FALSE
)
read_params_table(usm_xl_file, sheet = "USMs")
usm_csv_file <- download_usm_csv(
  file = "inputs_stics_example_USMs.csv",
  verbose = FALSE,
  stics_version = "V9.2"
)
read_params_table(file = usm_csv_file)
}
