#' @export
exists_param <- function(xml_doc_object,param_name, ...) {

  nb <- get_param_number(xml_doc_object,param_name, ...)

  if (is.list(nb)) {
    ex <- unlist(lapply(nb,as.logical))
    return(ex)
  }

  return(as.logical(nb))

}
