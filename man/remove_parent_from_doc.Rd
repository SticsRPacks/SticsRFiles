% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_parent_from_doc.R
\name{remove_parent_from_doc}
\alias{remove_parent_from_doc}
\title{Remove parent node of a parameter}
\usage{
remove_parent_from_doc(xml_doc, param_name, nodes_ids = NULL)
}
\arguments{
\item{xml_doc}{The XML document}

\item{param_name}{The parameter name}

\item{nodes_ids}{The node IDs to be removed (optional)}
}
\description{
Remove a parent node from an XML file.
}
\examples{
\dontrun{

xml_path <- file.path(get_examples_path( file_type = "xml"),"file_tec.xml")
tec_doc <- SticsRFiles:::xmldocument(xml_path)

# removing all the parent nodes the parameter belongs to
SticsRFiles:::remove_parent_from_doc(tec_doc, param_name = "julapI_or_sum_upvt")

# removing some of the parent nodes the parameter belongs to
SticsRFiles:::remove_parent_from_doc(tec_doc, param_name = "julapI_or_sum_upvt",
nodes_ids = c(1,3))

}

}
\keyword{internal}
