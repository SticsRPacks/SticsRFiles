is_stics_doc <- function(xml_doc, doc_type = NULL, doc_types = NULL) {
  #' @export

  # TODO : doc_types have been added as input arg, but not used yet, will be used
  # when doc_types will be defined against Stics Version
  # because they may change with versions ?

  if (is.null(doc_types)) {
    doc_types <- c("initialisations","usms", "sols", "fichiertec",
                   "fichiersta", "fichierplt", "fichierpar",
                   "fichierparamgen" )
  }

  if (! nargs() ) {
    return(doc_types)
  }

  # not an xmlDocument
  if (! is.xmlDocument(xml_doc) ) { return(FALSE) }

  if (!is.null(doc_type) && ! (doc_type %in% doc_types)) {
    warning(paste0("Not any tag name \"",doc_type,"\" for stics xml document!"))
    return(FALSE)
  }

  # checking doc_type against root name or
  # if doc_type is not given, if root name is in
  # doc types
  root_name <- xmlName(XML::xmlRoot(xml_doc@content))

  if (!is.null(doc_type) && ! doc_type == root_name ) {
    return(FALSE)
  }

  if (! root_name %in% doc_types ) {
    return(FALSE)
  }

  return(TRUE)

}


# individual functions for each xml Stics doc

is_stics_ini <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "initialisations"))
}

is_stics_usms <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "usms"))
}

is_stics_sols <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "sols"))
}

is_stics_tec <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "fichiertec"))
}

is_stics_sta <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "fichiersta"))
}

is_stics_plt <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "fichierplt"))
}

is_stics_par <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "fichierpar"))
}

is_stics_newpar <- function(xml_doc) {
  #' @export
  return(is_stics_doc(xml_doc, "fichierparamgen"))
}
