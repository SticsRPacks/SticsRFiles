
#' Test if a parameter exists
#'
#' @description Test if a parameter exists in an XML file
#'
#' @param xml_doc_object XML document
#' @param param_name The parameter name
#' @param ... Other parameters to pass to [get_param_number()].
#'
#@export
#'
exists_param <- function(xml_doc_object,param_name, ...) {

  nb <- get_param_number(xml_doc_object,param_name, ...)

  if (is.list(nb)) {
    ex <- unlist(lapply(nb,as.logical))
    return(ex)
  }

  return(as.logical(nb))

}
