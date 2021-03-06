% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/force_param_values.R
\name{force_param_values}
\alias{force_param_values}
\title{Generates files to force parameters values in Stics simulations}
\usage{
force_param_values(workspace, param_values)
}
\arguments{
\item{workspace}{Stics workspace path}

\item{param_values}{named vector of parameter values to force. See Details for more information.}
}
\value{
A logical status TRUE if successfull, FALSE otherwise
}
\description{
Generates a param.sti file and sets code optim in new_travail.usm to force parameters values in Stics simulations (this function is typically called before \code{SticsOnR::run_stics()})
}
\details{
This function operates on Stics text input files. Do not use it before calling \code{gen_usms_xml2txt()}, otherwise param.sti and new_travail.usm files will be overwritten.

This function has been created to be called before \code{SticsOnR::run_stics()}. It can not work with \code{SticsOnR::run_javastics()}, that will overwrite param.sti and new_travail.usm files.

param_values can contain NA. In this case, the corresponding parameter(s) will not be forced (default value(s), i.e. read in Stics input files, will be used).
If param_values==NA or param_values==NULL, not any parameter will be forced (all default values used).
}
\examples{
example_txt_dir <- get_examples_path(file_type="txt")
force_param_values(example_txt_dir, setNames(object=c(220,330), c("stlevamf", "stamflax")))

}
\seealso{
\code{SticsOnR::run_stics()}
}
