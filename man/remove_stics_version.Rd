% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage_stics_versions.R
\name{remove_stics_version}
\alias{remove_stics_version}
\title{Removing a version and data from the SticsRFiles library or package}
\usage{
remove_stics_version(
  version_name,
  delete_files = TRUE,
  location = "install",
  verbose = TRUE
)
}
\arguments{
\item{version_name}{name (i.e. "VX.Y") of the version to set
in the csv file containing informations about versions}

\item{delete_files}{Logical, TRUE for removing files (default), FALSE otherwise}

\item{location}{The destination where to remove information and data
"install" for removing things from the installed SticsRFiles library (default),
"package" for removing them from the package project (in RStudio)}

\item{verbose}{Logical, TRUE for displaying warnings (default), FALSE otherwise}
}
\value{
An invisible logical value, TRUE if successfull removing FALSE otherwise
}
\description{
Removing a version and data from the SticsRFiles library or package
}
\examples{
\dontrun{
SticsRFiles:::remove_stics_version(version_name = "V10.0")

SticsRFiles:::remove_stics_version(version_name = "V10.0",
                                   location = "package")

}
}
\keyword{internal}
