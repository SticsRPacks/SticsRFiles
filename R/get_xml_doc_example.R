#' @title Get an xmlDocument from a Stics xml file example
#' @param xml_name xml file name (see file names by calling get_xml_doc_example())
#'
#' @param stics_version the stics files version to use
#'
#' @export
get_xml_doc_example <- function(xml_name = NULL,
                                stics_version = "last") {

  # check/get version
  stics_version <- get_xml_stics_version(stics_version = stics_version)

  # stics_xml_types
  files <- list.files(pattern = ".xml$",
                      path = system.file( paste0("extdata/xml/examples/",stics_version),
                                          package = "SticsRFiles"))

  if ( base::is.null(xml_name) ) {
    return(files)
  }

  if ( ! xml_name %in% files ) {
    stop("File does not exist in package examples, run get_xml_example_doc() to get the list !")
  }

  # getting a default xmldocument object template
  xml_file <- paste0("extdata/xml/examples/",stics_version,"/",xml_name)
  xml_doc_object <- xmldocument(system.file(xml_file, package = "SticsRFiles"))

  return(xml_doc_object)

}
