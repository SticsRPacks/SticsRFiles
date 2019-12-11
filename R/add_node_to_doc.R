#' Add node to document
#'
#' @param xml_doc XML document
#' @param new_node New node to add
#' @param nodes_nb Nodes number
#' @param parent_path Path to the parent node
#'
#@export
#'
add_node_to_doc <- function(xml_doc, new_node, nodes_nb = 1, parent_path ) {
  replicate(nodes_nb, addNodes(xml_doc,xmlClone(new_node), parent_path ))
}
