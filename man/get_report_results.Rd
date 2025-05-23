% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_report_results.R
\name{get_report_results}
\alias{get_report_results}
\title{Extracting data from the STICS report file}
\usage{
get_report_results(
  workspace,
  file_name = "mod_rapport.sti",
  usm = NULL,
  var_list = NULL,
  usm_name = lifecycle::deprecated()
)
}
\arguments{
\item{workspace}{Path of the directory containing the STICS report file
to read.}

\item{file_name}{A report file name among "mod_rapport.sti" (default),
"mod_rapportA.sti", "mod_rapportP.sti"}

\item{usm}{Vector of USM names. Optional, if not provided, the function
returns the results for all USMs.}

\item{var_list}{vector of output variables names to filter
(optional, see \code{get_var_info()} to get the names of the variables)}

\item{usm_name}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} \code{usm_name} is no
longer supported, use \code{usm} instead.}
}
\value{
A data.frame
}
\description{
Extracting data from the STICS report file
}
\details{
The data may be filtered using \code{usm_name} vector of usm names and
and/or \code{var_list} vector of variables names. In the returned data.frame,
variables names respect the same syntax as in the get_sim output.
}
\examples{
path <- get_examples_path(file_type = "sti")
get_report_results(workspace = path)

get_report_results(workspace = path, usm = c("DurumWheat", "grass"))

get_report_results(workspace = path, var_list = c("masec(n)", "QNplante"))

get_report_results(workspace = path, usm = c("DurumWheat", "grass"))

get_report_results(workspace = path)

get_report_results(workspace = path, file_name = "mod_rapportA.sti")

}
