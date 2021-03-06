% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_stics_nodes.R
\name{add_stics_nodes}
\alias{add_stics_nodes}
\title{Add node to a Stics XML document}
\usage{
add_stics_nodes(
  xml_doc,
  formalism_name = NULL,
  nodes_nb = 1,
  stics_version = "last"
)
}
\arguments{
\item{xml_doc}{XML document}

\item{formalism_name}{Name of the formalism (optional)}

\item{nodes_nb}{number of operations to add}

\item{stics_version}{The version of STICS (eg "V9.1")}
}
\description{
Add node to a Stics XML document
}
\examples{
\dontrun{

tec_xml <- file.path(get_examples_path( file_type = "xml" ), "file_tec.xml")
tec_doc <- SticsRFiles:::xmldocument(tec_xml)

# Adding one irrigation operation
SticsRFiles:::add_stics_nodes(tec_doc, "irrigation")

# Adding three irrigation operations
SticsRFiles:::add_stics_nodes(tec_doc, "irrigation", nodes_nb = 3)

}

}
\keyword{internal}
