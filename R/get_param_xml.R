#' @title Getting parameter values from xml files
#'
#' @description Extracting parameter values for a list of xmlDocument and
#' of parameters
#'
#' @param xml_files an xml file, or a vector/list of
#' @param param_names_list parameter names list (i.e.: option parameter name, parameter name,
#' other kinds in ini and some tec parameters not taken into account for the moment)
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param show_xpath Logical for displaying or not xpath value
#'
#' @return A list of parameter list length, with parameters values for each xml file
#'
#' @examples
#' \dontrun{
#' xml_path = system.file("extdata/xml/examples/V9.0/sols.xml", package = "SticsRFiles")
#' SticsRFiles:::get_param_xml(xml_path, "argi")
#' SticsRFiles:::get_param_xml(xml_path, c("argi", "norg"))
#'
#' SticsRFiles:::get_param_xml(xml_path, "argi",
#' parent_name = "sol", parent_sel_attr = "solcanne")
#'
#' SticsRFiles:::get_param_xml(xml_path, c("argi", "norg"),
#' parent_name = "sol", parent_sel_attr = c("solcanne", "solbanane"))
#'
#' SticsRFiles:::get_param_xml(list(xml_path, xml_path), c("argi","norg"),
#' parent_name = "sol", parent_sel_attr = c("solcanne","solbanane"))
#'
#' }
#'
#' @export
get_param_xml <- function(xml_files,param_names_list,
                              parent_name = NULL, parent_sel_attr = NULL,
                              show_xpath = FALSE){


  xml_docs <- lapply(xml_files,xmldocument)

  values <- get_param_value(xml_docs, param_name = param_names_list,
                              parent_name = parent_name,
                              parent_sel_attr = parent_sel_attr,
                              show_xpath = show_xpath)


  return(values)


}
