#' @title Get a list of Stics xml parameters names an xmlDocument node
#' @param xml_node an xml XMLInternalElementNode
#' @param param_list param names vector, used for recursive calls
#'
#' @return a character vector of parameters names
#'
#' @export
get_params_names <- function(xml_node,param_list = c()) {

  # TODO
  # - for ini files
  # - for all: add an input parameter for specifying which formalism to use
  # to restrict params names list !

  if ( ! class(xml_node) == "XMLInternalElementNode") {
    stop("The document is not an xml node !")
  }

  node_name <- xmlName(xml_node)

  # getting only one usm node
  if ( node_name == "usms" && unique_val ) {
    xml_node <- xmlChildren(xmlRoot(xml_node))[[1]]
  }

  childs <- xmlChildren(xml_node)
  childs_names <- names(childs)

  node_name <- xmlName(xml_node)

  #parent_name <- xmlName(xmlParent(xml_node))


  childs_nb <- length(childs)


  if (node_name =="option") {
    #param_list <- c(param_list,)
    #print(xmlAttrs(xml_node)["nomParam"])
    attr_name <- "nomParam"
  }

  if (node_name =="param") {
    #param_list <- c(param_list,)
    #print(xmlAttrs(xml_node)["nom"])
    attr_name <- "nom"
  }

  if (node_name =="colonne" &&
      ( xmlName(xmlParent(xml_node)) %in% c("ta_entete", "tableau_entete") ) ) {
    attr_name <- "nom"

  }

  if ( ! exists("attr_name") ) {
    attr_name <- "none"
  }

  # Adding the param name t the list if it does not exist
  if ( attr_name %in% names(xmlAttrs(xml_node))) {
    param_name <- xmlAttrs(xml_node)[attr_name]
    #print(param_name)
    if ( ! param_name %in% param_list ) param_list <- c(param_list, param_name)
  }


  # TODO : see if it is usefull, look in the loop !
  #if (!childs_nb ) return()

  #if (childs_names == "text") return()

  if ( (!childs_nb ) || (length(childs_names) == 1 && childs_names == "text") ) {
    return(param_list)
  }



  for (n in 1:childs_nb) {

    if ( ! class(childs[[n]]) == "XMLInternalElementNode") {
      #print(class(childs[[n]]))
      next
    }
    param_list <- get_params_names(childs[[n]], param_list)
  }

  names(param_list) <- NULL

  return(param_list)
}
