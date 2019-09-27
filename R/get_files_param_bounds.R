#' @export
get_files_param_bounds <- function(xml_file, param_name, bounds_name = NULL) {
  library("Classes")


  if (length(xml_file) > 1 ) {

    param_bounds <- lapply(xml_file,
                           function(x) get_files_param_bounds(x,
                                                              param_name,
                                                              bounds_name))
    return(param_bounds)
  }

  xml_doc <- xmldocument(xml_file)

  param_bounds <- get_param_bounds(xml_doc, param_name, bounds_name)

  return(param_bounds)

}
