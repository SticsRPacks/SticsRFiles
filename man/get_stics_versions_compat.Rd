% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stics_versions_compat.R
\name{get_stics_versions_compat}
\alias{get_stics_versions_compat}
\title{Get the compatible STICS versions}
\usage{
get_stics_versions_compat(version_index = NULL)
}
\arguments{
\item{version_index}{Absolute positive index, or negative relative index from
latest version}
}
\value{
A named list with the STICS versions compatible with this package
($versions_list), and the latest version in use ($latest_version) or
an existing version selected using version_index.
}
\description{
Get the versions of STICS that are fully compatible
with this package.
}
\examples{
# Getting the complete versions list
get_stics_versions_compat()

# Getting the first version
get_stics_versions_compat(1)

# Getting the previous version of the latest one
get_stics_versions_compat(-1)

}
