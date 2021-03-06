% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage_stics_versions.R
\name{download_csv_files}
\alias{download_csv_files}
\title{Dowloading inputs and/or outputs csv files from a repository branch/tag}
\usage{
download_csv_files(
  branch_url,
  dest_dir,
  file_name = "all",
  ids = get_svn_identifiers(),
  overwrite = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{branch_url}{Address of the branch or tag}

\item{dest_dir}{Directory path where to store files}

\item{file_name}{File name(s)
or keyword "all" for both files (default)}

\item{ids}{Connexion identifiers to the subversion server}

\item{overwrite}{Logical, TRUE for overwtiting files, FALSE otherwise}

\item{verbose}{TRUE to display warnings (default), FALSE otherwise}
}
\description{
Dowloading inputs and/or outputs csv files from a repository branch/tag
}
\examples{
\dontrun{
SticsRFiles:::download_csv_files(
  branch_url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10",
  dest_dir = system.file("extdata", package = "SticsRFiles"))
}
}
\keyword{internal}
