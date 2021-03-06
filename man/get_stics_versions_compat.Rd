% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stics_versions_compat.R
\name{get_stics_versions_compat}
\alias{get_stics_versions_compat}
\title{Get the compatible Stics versions list}
\usage{
get_stics_versions_compat()
}
\value{
A named list with the STICS versions compatible with this package
($versions_list), and the last version in use ($last_version).
}
\description{
Get the versions of stics that are fully compatible
with this package.
}
\examples{
\dontrun{
SticsRFiles::get_stics_versions_compat()

#> $versions_list
#> [1] "V8.5" "V9.0" "V9.1"
#>
#> $last_version
#> [1] "V9.1"
}

}
