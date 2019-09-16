#' @export
remove_node_from_doc <- function( xml_doc, param_name ,
                                  parent_path = NULL,
                                  remove_parent = FALSE,
                                  nodes_ids = NULL ) {

  xpath_node <- get_param_type(xml_doc = xml_doc,
                               param_name = param_name,
                               parent_name = parent_path)$xpath

  # if type does not exist
  if (is.null(xpath_node)){
    print(paste("Unknown parameter in xml doc: ",param_name))
    return(invisible())
  }


  # if we remove the parent node
  if (remove_parent) {
    # getting the nodes for intervention to which the param_name belongs
    xml_nodes <- lapply(getNodeS(xml_doc,xpath_node),xmlParent)
  } else {
    # getting nodes
    xml_nodes <- getNodeS(xml_doc, xpath_node)
  }

  if (is.null(xml_nodes)){
    print("No nodes to remove from xml doc !")
    return(invisible())
  }

  nodes_nb <- length(xml_nodes)
  # selecting nodes with nodes_ids
  if (! is.null(nodes_ids) && max(nodes_ids) <= nodes_nb) {
    xml_nodes <- xml_nodes[nodes_ids]
  }

  removeNodes(xml_nodes)

  return(invisible(xml_doc))

}
