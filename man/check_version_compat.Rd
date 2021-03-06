% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stics_versions_compat.R
\name{check_version_compat}
\alias{check_version_compat}
\title{Checking the validity of a given version code}
\usage{
check_version_compat(version_name = "last")
}
\arguments{
\item{version}{An optional version name as listed in
get_stics_versions_compat() return}
}
\value{
A valid version string
}
\description{
Checking the validity of a given version code
}
\examples{
\dontrun{

SticsRFiles:::check_version_compat()

[1] "V9.1"

check_version_compat(version = "V8.5")
[1] "V8.5"

}
}
\keyword{internal}
