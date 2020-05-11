#' @title Getting parameter values from xml files
#'
#' @description Extracting parameter values for a list of xml files and parameters
#'
#' @param xml_file An xml file, or a vector/list of
#' @param param_name parameter names vector (i.e.: parameter name or option code, optional)
#' @param select node name or attribute name to use for selection (optional, default to no selection)
#' @param value value used for select (optional)
#' @param ... Pass further arguments to `get_param_value()`
#'
#' @return A list of parameter values for each xml_file (a list of list)
#'
#' @examples
#' xml_path = file.path(get_examples_path( file_type = "xml"),"sols.xml")
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
#' @export
get_param_xml <- function(xml_file,
                          param_name=NULL,
                          select = NULL,
                          value = NULL,
                          ...) {
  # ... argument for passing : ids, show_xpath to get_param_value


  xml_docs <- lapply(xml_file,xmldocument)

  values <- get_param_value(xml_doc = xml_docs,
                            param_name = param_name,
                            parent_name = select,
                            parent_sel_attr = value,
                            ...)
  xml_names= lapply(xml_file, basename)%>%unlist()

  # If there are duplicated names in xml_file:
  is_duplicated_name= xml_names%>%duplicated()
  xml_names[is_duplicated_name]= paste0("xml_",which(is_duplicated_name==TRUE),"_",xml_names[is_duplicated_name])

  names(values) <- xml_names

  return(values)
}
