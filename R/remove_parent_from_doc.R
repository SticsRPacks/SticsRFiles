
#' Remove parent node
#'
#' @description Remove a parent node from an XML file.
#'
#' @param xml_doc The XML document
#' @param param_name The parent name
#' @param parent_path The parent path
#' @param remove_parent Boolean. Do we remove the parent node?
#' @param nodes_ids The requested node IDs.
#'
#' @export
remove_parent_from_doc <- function( xml_doc, param_name ,
                                  parent_path = NULL,
                                  remove_parent = FALSE,
                                  nodes_ids = NULL ) {

  remove_node_from_doc(xml_doc = xml_doc, param_name = param_name,
                       parent_path = parent_path, remove_parent = TRUE,
                       nodes_ids = nodes_ids)

  return(invisible(xml_doc))

}
