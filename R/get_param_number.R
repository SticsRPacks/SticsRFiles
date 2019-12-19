#' @title Get the number of a (or alist of) Stics xml parameter(s) occurence from a request in an xmlDocument
#' @param xml_doc_object an xmlDocument object (created from an xml file)
#'
#' @param param_name parameter name or a vector of names
#' @param ... other arguments that can be transmitted to get_param_value function (see doc)
#'
#' @return a numeric vector
#'
#' @keywords internal
#'
# TODO: may use get_param_types returning values nb in $length field
get_param_number <- function(xml_doc_object,param_name,...) {

  values <- get_param_value(xml_doc_object,param_name,...)

  if (is.list(values)) {
    nb <- unlist(lapply(values,length), use.names = FALSE)
    return(nb)
  }

  return(length(values))
}
