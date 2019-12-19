#' @title Generate from a template or modify a Stics sols or usms xmlDocument

#' @param doc_type Document type
#' @param xml_doc  The xml document
#' @param nodes_nb The number of nodes
#' @param nodes_param Node parameter
#' @param stics_version Version of the STICS model
#' @param overwrite  Overwrite the document values ?
#'
#' @keywords internal
#'
gen_xml_doc <- function(doc_type, xml_doc = NULL,
                        nodes_nb = NULL, nodes_param = NULL,
                        stics_version = "last", overwrite = F) {


  # for usms and sols files


  keep_existing = T

  # check/get version of templates xml files
  stics_version <- get_xml_stics_version(stics_version = stics_version)


  doc_types <- list()
  doc_types$usms <- list(root="usms",node="usm")
  doc_types$sols <- list(root="sols",node="sol")

  if ( ! nargs()) {
    return(names(doc_types))
  }

  if ( ! is.element(doc_type, names(doc_types)) ) {
    stop(paste0("The doc type is not an existing one: ",doc_type))
  }

  root <- doc_types[[doc_type]]$root
  node <- doc_types[[doc_type]]$node

  root_str <- paste0("/",root)
  node_str <- paste0("//",node)

  # getting a default xml template
  if ( base::is.null(xml_doc) ) {
    # using function get_xml_base_doc
    xml_doc <- get_xml_base_doc(xml_type = doc_type,
                                stics_version = stics_version)
    overwrite = T
    keep_existing = F
  }

  elts_nb <- NULL

  # identity or single usms/sols doc from the template file
  if ( all(base::is.null(c(nodes_nb, nodes_param))) ) {
    return(xml_doc)
  }

  # calculating nodes number
  if (! base::is.null(nodes_nb)) { elts_nb = nodes_nb }

  if ( "data.frame" %in% class(nodes_param) ) {
    elts_nb <- dim(nodes_param)[1]
  }

  # checking nodes number value
  if (base::is.null(elts_nb)) {
    stop("Error in usm number or in soils parameters table !")
  }

  # getting usm/sol nodes
  xml_nodes <- getNodeS(xml_doc, node_str)

  # Nothing to do
  doc_nodes_nb <- length(xml_nodes)
  if ( doc_nodes_nb == elts_nb && base::is.null(nodes_param) ) {
    return(xml_doc)
  }

  if (doc_nodes_nb < elts_nb ) overwrite = T

  # Creating doc structure from a base node
  if ( overwrite ) {

    # Keeping only one usm node in the xml document
    if ( doc_nodes_nb > 1) removeNodes(xml_nodes[2:doc_nodes_nb])

    if ( keep_existing ) {
      add_node_to_doc(xml_doc, xml_nodes[[1]], nodes_nb = (elts_nb - 1), paste0("//",root))
    } else {
      removeNodes(xml_nodes[1])
      add_stics_nodes(xml_doc = xml_doc, file_tag = root, nodes_nb = elts_nb)
    }

  }

  if ( base::is.null(nodes_param) ) {
    return(xml_doc)
  }

  # setting data according to doc_type
  switch( doc_type,
          usms = set_usms_param_xml(xml_doc, nodes_param, overwrite = overwrite),
          sols = set_sols_param_xml(xml_doc, nodes_param, overwrite = overwrite)
  )



  return(xml_doc)

}
