#' Is it a STICS xml document
#'
#' @description Test if an XML document is a STICS document.
#'
#' @param xml_doc An xml document
#' @param doc_type The type of xml document
#' @param doc_types The different possible document types (optional)
#'
#' @return A logical value giving if xml_doc is a Stics xmlDoxument object
#' (TRUE, with document type as attribute "type"), or not (FALSE)
#'
#'
#' @examples
#' \dontrun{
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#' sols_doc <- SticsRFiles:::xmldocument(xml_path)
#' SticsRFiles:::is_stics_doc(sols_doc)
#' }
#'
#' @keywords internal
#'
is_stics_doc <- function(xml_doc, doc_type = NULL, doc_types = NULL) {

  # TODO : doc_types have been added as input arg, but not used yet,
  # will be used when doc_types will be defined against Stics Version
  # because they may change with versions ?

  if (base::is.null(doc_types)) {
    doc_types <- c(
      "initialisations", "usms", "sols", "fichiertec",
      "fichiersta", "fichierplt", "fichierpar",
      "fichierparamgen"
    )
  }

  if (!nargs()) {
    return(doc_types)
  }

  # not an xml_document
  if (!is.xml_document(xml_doc)) {
    return(FALSE)
  }

  if (!base::is.null(doc_type) && !(doc_type %in% doc_types)) {
    warning(paste0("Not any tag name \"",
                   doc_type,
                   "\" for stics xml document!"))
    return(FALSE)
  }

  # checking doc_type against root name or
  # if doc_type is not given, if root name is in
  # doc types
  root_name <- XML::xmlName(XML::xmlRoot(xml_doc@content))

  if (!base::is.null(doc_type) && !doc_type == root_name) {
    return(FALSE)
  }

  type_idx <- doc_types %in% root_name

  if (!any(type_idx)) {
    return(FALSE)
  }

  ret <- TRUE
  attr(ret, "type") <- root_name
  return(ret)
}



#' @rdname is_stics_doc
# @export
is_stics_ini <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "initialisations"))
}

#' @rdname is_stics_doc
# @export
is_stics_usms <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "usms"))
}

#' @rdname is_stics_doc
# @export
is_stics_sols <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "sols"))
}

#' @rdname is_stics_doc
# @export
is_stics_tec <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "fichiertec"))
}

#' @rdname is_stics_doc
# @export
is_stics_sta <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "fichiersta"))
}

#' @rdname is_stics_doc
# @export
is_stics_plt <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "fichierplt"))
}

#' @rdname is_stics_doc
# @export
is_stics_par <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "fichierpar"))
}

#' @rdname is_stics_doc
# @export
is_stics_newpar <- function(xml_doc) {
  return(is_stics_doc(xml_doc, "fichierparamgen"))
}
