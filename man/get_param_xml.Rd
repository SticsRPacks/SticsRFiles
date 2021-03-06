% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_param_xml.R
\name{get_param_xml}
\alias{get_param_xml}
\title{Getting parameter values from xml files}
\usage{
get_param_xml(xml_file, param_name = NULL, select = NULL, value = NULL, ...)
}
\arguments{
\item{xml_file}{An xml file, or a vector/list of}

\item{param_name}{parameter names vector (i.e.: parameter name or option code, optional)}

\item{select}{node name or attribute name to use for selection (optional, default to no selection)}

\item{value}{value used for select (optional)}

\item{...}{Pass further arguments to \code{get_param_value()}}
}
\value{
A list of parameter values for each xml_file (a list of list)
}
\description{
Extracting parameter values for a list of xml files and parameters
}
\examples{
\dontrun{
# Soil file
xml_path = file.path(get_examples_path( file_type = "xml"),"sols.xml")

# For all soils
get_param_xml(xml_path)
get_param_xml(xml_path, "argi")
get_param_xml(xml_path, c("argi", "norg"))

# For one soil selection
get_param_xml(xml_path, "argi",
select = "sol", value = "solcanne")

# For soils and parameters vectors
# scalar parameters per soil
get_param_xml(xml_path, c("argi", "norg"),
select = "sol", value = c("solcanne", "solbanane"))

$sols.xml
$sols.xml$argi
[1] 30.2 70.0

$sols.xml$norg
[1] 0.27 0.20

# vector parameters per soil (5 values, one per soil layer)
get_param_xml(xml_path, c("epc", "HCCF"),
select = "sol", value = c("solcanne", "solbanane"))

$sols.xml
$sols.xml$epc
[1] 20 20 20 20 20 10 10 10 10 40

$sols.xml$HCCF
[1] 46.8 46.4 48.5 50.1 50.1 41.0 41.0 41.0 42.0 42.0

# Crop management file
xml_path = file.path(get_examples_path( file_type = "xml"),"file_tec.xml")

get_param_xml(xml_path)

# Getting parameters for irrigation (date and quantity)
get_param_xml(xml_path, c("julapI_or_sum_upvt", "amount"))


# Getting all parameters for a given formalism: "irrigation"
get_param_xml(xml_path, select = "formalisme", value = "irrigation")

}
}
