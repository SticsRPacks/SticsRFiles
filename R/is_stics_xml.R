#' Is it a STICS xml file
#'
#' @description Test if an XML file is a STICS one.
#'
#' @param xml_path An xml file path
#' @param file_type The type of the xml file
#'
#' @return A logical value giving if input xml file is a Stics one
#' (TRUE), or not (FALSE)
#'
#'
#' @examples
#' \dontrun{
#' xml_path = file.path(get_examples_path( file_type = "xml"), "sols.xml")
#' SticsRFiles:::is_stics_xml(xml_path)
#' }
#'
#'
#' @keywords internal
#'
is_stics_xml <- function(xml_path, file_type = NULL) {

  if ( dir.exists(xml_path) | !file.exists(xml_path) ) {
    return(FALSE)
  }

  xml_doc <- xmldocument(xml_path)

  return(is_stics_doc(xml_doc, doc_type = file_type ))
}

#' @rdname is_stics_xml
# @export
is_ini_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "initialisations"))
}

#' @rdname is_stics_xml
# @export
is_usms_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "usms"))
}

#' @rdname is_stics_xml
# @export
is_sols_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "sols"))
}

#' @rdname is_stics_xml
# @export
is_tec_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "fichiertec"))
}

#' @rdname is_stics_xml
# @export
is_sta_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "fichiersta"))
}

#' @rdname is_stics_xml
# @export
is_plt_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "fichierplt"))
}

#' @rdname is_stics_xml
# @export
is_par_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "fichierpar"))
}

#' @rdname is_stics_xml
# @export
is_newpar_xml <- function(xml_path) {
  return(is_stics_xml(xml_path, "fichierparamgen"))
}

get_xml_type <- function(xml_path) {
  is_xml <- is_stics_xml(xml_path)
  if (!is_xml) return(NULL)

  return(attr(is_xml,"type"))
}
