#' @title Get a list of Stics xml parameters types and xpath from a request in an xmlDocument
#' @param xml_doc an xmlDocument object (created from an ini file)
#' @param param_names parameters names
#'
#' @return a list of list of parameter type, xpath and values number
#'
#'
# TODO : may be merged with get_param_type !
get_params_types <- function(xml_doc, param_names) {

  # returning basic types, if call with no arguments
  if ( !nargs()) {
    return(get_param_type())
  }

  if (is.vector(param_names)) {
    param_names <- list(param_names)
  }
  return(lapply(param_names, function(x) get_param_type(xml_doc,x)))
}
