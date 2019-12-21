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
#' get_param_xml(path/to/xml/file,"codetemp")
#' get_param_xml(c(path/to/xml/file1,path/to/xml/file2),"codetemp")
#' get_param_xml(path/to/xml/file,c("codetemp","codegdh"))
#' get_param_xml(c(path/to/xml/file1,path/to/xml/file2),c("codetemp","codegdh"))
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
