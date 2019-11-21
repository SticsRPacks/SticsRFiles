#' @export
add_node_to_doc <- function(xml_doc, new_node, nodes_nb = 1, parent_path ) {


  replicate(nodes_nb, addNodes(xml_doc,xmlClone(new_node), parent_path ))


}
