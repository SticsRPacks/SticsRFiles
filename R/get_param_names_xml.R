#' @title Getting parameters names from xml files
#'
#' @description Extracting parameters names for an xml files or a vector of files.
#'
#' @param xml_file an xml file path or a vector of paths
#'
#' @param mult extra argument for detecting a single file use
#'
#' @return A list of parameters names list
#'
#' @examples
#' \dontrun{
#' library(SticsRFiles)
#' xml_file <- "path/to/xmlfile"
#' xml_files_list <- c("path/to/xmlfile1", "path/to/xmlfile2")
#'
#' param_names <- get_param_names_xml(xml_file)
#'
#' param_names <- get_param_names_xml(xml_files_list)
#' }
#'
#' @export
#'
get_param_names_xml <- function(xml_file, mult=FALSE) {


  if ( length(xml_file) > 1 ) {

    param_names <- lapply(xml_file,
                           function(x) get_param_names_xml(x, mult=TRUE))

    return(param_names)
  }

  xml_doc <- xmldocument(xml_file)

  param_names <- get_params_names(xml_doc)

  if (! mult ) param_names <- list(param_names)

  return(param_names)

}
