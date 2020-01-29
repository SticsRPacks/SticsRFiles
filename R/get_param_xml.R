#' @title Getting parameter values from xml files
#'
#' @description Extracting parameter values for a list of xmlDocument and
#' of parameters
#'
#' @param xml_files an xml file, or a vector/list of
#' @param param_names parameter names list (i.e.: option parameter name, parameter name,
#' other kinds in ini and some tec parameters not taken into account for the moment)
#' @param select node name or attribute name name to use for selection  (optional)
#' @param value value used for select (optional)
#' @param ... To pass some other arguments
#'
#' @return A list of parameter list length, with parameters values for each xml file
#'
#' @examples
#' \dontrun{
#' xml_path = system.file("extdata/xml/examples/V9.1/sols.xml", package = "SticsRFiles")
#' get_param_xml(xml_path, "argi")
#' get_param_xml(xml_path, c("argi", "norg"))
#'
#' get_param_xml(xml_path, "argi",
#' select = "sol", value = "solcanne")
#'
#' get_param_xml(xml_path, c("argi", "norg"),
#' select = "sol", value = c("solcanne", "solbanane"))
#'
#' get_param_xml(list(xml_path, xml_path), c("argi","norg"),
#' select = "sol", value = c("solcanne","solbanane"))
#'
#' }
#'
#' @export
get_param_xml <- function(xml_files,
                          param_names,
                          select = NULL,
                          value = NULL,
                          ...) {
  # ... argument for passing : ids, show_xpath to get_param_value


  xml_docs <- lapply(xml_files,xmldocument)

  values <- get_param_value( xml_doc = xml_docs,
                            param_name = param_names,
                            parent_name = select,
                            parent_sel_attr = value,
                            ...)



  return(values)


}
