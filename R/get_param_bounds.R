#' @export
get_param_bounds <- function(xml_doc, param_name, bounds_name = NULL) {
  library("Classes")

  def_names <- c("min","max")

  if (is.null(bounds_name)) {
    bounds_name <- def_names
  } else if ( ! all(is.element(bounds_name,def_names )) ) {
    stop("Names are not consitent with file bound names")
  }

  if ( length(param_name) > 1) {

    return(lapply(as.list(param_name), function(x) get_param_bounds(xml_doc = xml_doc
                                                              , x,
                                                              bounds_name = bounds_name)))
  }

  param_type <- get_param_type(xml_doc, param_name)
  exist_param <- !is.null(param_type$xpath)

  if (! exist_param ) return(NULL)



  bounds_values <- lapply(bounds_name, function(x)
    Classes::getAttrsValues(xml_doc,param_type$xpath,x))


  return(as.numeric(unlist(bounds_values)))


}
