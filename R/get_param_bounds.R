#' @title Getting parameters bounds from an xmlDocument object
#'
#' @description Extracting parameters min and/or max bounds for a parameter
#' or a vector of parameters from an xmlDocument class object.
#'
#' @param xml_doc an xmlDocument class object
#' @param param_name a parameter name of a vector of parameters names
#' @param bounds_name bounds name "min" or "max" (optional, default value c("min","max ))
#'
#' @return A list of parameters bounds values
#'
#' @examples
#' library(SticsRFiles)
#' xml_doc <- xmldocument("path/to/xmlfile")
#' param_bounds <- get_param_bounds(xml_doc, "param", "min")
#'
#' param_bounds <- get_param_bounds(xml_doc, c("param1", "param2"), "max")
#'
#' param_bounds <- get_param_bounds(xml_doc, c("param1", "param2"))
#'
#' @export
#'
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
