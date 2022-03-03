#' @title Get a list of Stics xml parameters names from nodes names in an xmlDocument
#' @param xml_node an xml XMLInternalElementNode
#'
#' @param param_list param names vector, used for recursive calls
#' @param unique_val logical, TRUE to get unique names list, FALSE otherwise
#'
#' @return a character vector of parameters names
#'
#' @keywords internal
#'
get_params_from_doc_node <- function(xml_node, param_list = c(), unique_val = TRUE) {

  # for ini, usms files

  if (!class(xml_node) == "XMLInternalElementNode") {
    stop("The document is not an xml node !")
  }

  node_name <- xmlName(xml_node)

  # getting only one usm node
  if (node_name == "usms" && unique_val) {
    xml_node <- xmlChildren(xmlRoot(xml_node))[[1]]
  }

  childs <- xmlChildren(xml_node)
  childs_names <- names(childs)

  if (length(childs_names) == 1 && childs_names == "text") {
    param_list <- c(param_list, xmlName(xml_node))
  } else {
    for (n in childs_names) {
      param_list <- get_params_from_doc_node(childs[[n]], param_list)
    }
  }

  return(param_list)
}
