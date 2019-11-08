#' @title Get a list of Stics xml parameters names an xmlDocument node
#' @param xml_node an xml XMLInternalElementNode
#' @param param_list param names vector, used for recursive calls
#' @param full_list TRUE or getting all names, FALSE otherwise (default)
#' for unique names list
#'
#' @return a character vector of parameters names
#'
#' @export
get_params_names <- function(xml_node,param_list = c(), full_list = FALSE) {

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

  attr_name <- "none"

  if (node_name =="option") {
    attr_name <- "nomParam"
  }

  if (node_name =="param") {
    attr_name <- "nom"
  }

  # selecting param names : full for all values
  tab_names <- c("ta_entete", "tableau_entete")
  if (full_list) {
    tab_names <- c("ta", "tableau")
  }

  if (node_name =="colonne" &&
      ( xmlName(xmlParent(xml_node)) %in% tab_names ) ) {
    attr_name <- "nom"
  }

  # The param name is the node name
  # but not "plante" or "horizon"
  if ( attr_name == "none" &&
       ! ( node_name %in% c("plante", "horizon", "initialisations", "sol")) ) {
    param_list <- c(param_list, node_name)
  }


  # Adding the param name t the list if it does not exist
  # or if a full list is asked
  if ( attr_name %in% names(xmlAttrs(xml_node))) {
    param_name <- xmlAttrs(xml_node)[attr_name]
    if ( full_list || ! (param_name %in% param_list )) {
      param_list <- c(param_list, param_name)
      #print(param_name)
    }
  }






  # TODO : see if it is usefull, look in the loop !
  #if (!childs_nb ) return()

  # for a text node
  if ( (!childs_nb ) || (length(childs_names) == 1 && childs_names == "text") ) {
    return(param_list)
  }



  # loop over childs
  for (n in 1:childs_nb) {

    if ( ! class(childs[[n]]) == "XMLInternalElementNode") {
      #print(class(childs[[n]]))
      next
    }
    param_list <- get_params_names(childs[[n]], param_list, full_list = full_list)
  }

  names(param_list) <- NULL

  return(param_list)
}
