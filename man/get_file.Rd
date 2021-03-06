% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_file.R
\name{get_file}
\alias{get_file}
\title{Read STICS observation or simulation files (.obs or mod_s)}
\usage{
get_file(
  workspace,
  usm_name = NULL,
  var_list = NULL,
  dates_list = NULL,
  usms_filepath = NULL,
  javastics_path = NULL,
  verbose = TRUE,
  type = c("sim", "obs")
)
}
\arguments{
\item{workspace}{Path of a JavaStics workspace, or a vector of.}

\item{usm_name}{Vector of usms to read (optional, used to filter usms)}

\item{var_list}{vector of output variables names to filter (optional, see \code{get_var_info()} to get the names of the variables)}

\item{dates_list}{list of dates to filter (optional, should be a POSIX date)}

\item{usms_filepath}{Path of the usms file (optional)}

\item{javastics_path}{JavaStics installation path (optional, needed if the plant files are not in the \code{workspace}
but rather in the JavaStics default workspace). Only used to get the plants names.}

\item{verbose}{Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)}

\item{type}{The type of file to read, either "obs" or "sim".}
}
\value{
A named list of \code{data.frame}s with observations or simulation data. The list elements are named after
the usms names.
}
\description{
Read STICS observation or simulation files from a JavaStics workspace and store data into a list per usm.
Used by \code{get_obs()} and \code{get_sim()}. Operate first computation and then call \code{get_file_()}.
}
\details{
The \code{.obs} files names should match USMs mames, e.g. for a usm called "banana",
the \code{.obs} file should be named \code{banana.obs}. For intercrops, the name should be suffixed by "p" for
the principal and "a" for the associated plant.
}
\keyword{internal}
