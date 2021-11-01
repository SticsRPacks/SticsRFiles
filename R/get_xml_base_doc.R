#' @title Get an xmlDocument from a Stics xml file template
#' @param xml_type xml file type (see types returned when calling get_xml_base_doc())
#'
#' @param stics_version the stics files version to use
#'
#' @return an xmlDocument object
#'
#' @examples
#' \dontrun{
#' # Getting xml Stics files types list (i.e. keywords)
#' SticsRFiles:::get_xml_base_doc()
#'
#' # Getting a soil document with one soil definition
#' SticsRFiles:::get_xml_base_doc("sols")
#'
#' # Stics version can be provided, V9.1
#' # corresponds to stics_version = "latest"
#' # View available Stics files version
#' SticsRFiles:::get_xml_stics_version()
#' # Giving Stics version
#' SticsRFiles:::get_xml_base_doc("sols", stics_version = "V9.1")
#'
#' }
#'
#' @keywords internal
#'
get_xml_base_doc <- function(xml_type = NULL,
                             stics_version = "latest") {

  # types list
  types <- c("sols","usms","ini","tec","sta")
  # returning types if no args
  if (! nargs()) {
    return(types)
  }

  # index for getting files_pref value
  idx <- types %in% xml_type
  # checking the xml_type
  if (! any(idx)) {
    stop("Unknown xml type for getting an xml template xmlDocument !")
  }

  # check & get version
  stics_version <- get_xml_stics_version(stics_version = stics_version)

  # getting files prefix
  files_pref <- c("one","one","file","file","file")
  pref <- files_pref[idx]

  # getting a default xmldocument object template
  tmpl_file <- file.path(get_examples_path( file_type = "xml_tmpl", version_name = stics_version),
                         paste0(pref,"_",xml_type,".xml"))
  xml_doc_object <- xmldocument(tmpl_file)

  return(xml_doc_object)

}
