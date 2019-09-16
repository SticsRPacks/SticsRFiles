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
