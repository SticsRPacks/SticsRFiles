#' @title Get a list of Stics xml parameters names an xmlDocument or  XML node
#' @param xml_object an xml XMLInternalElementNode or xmlDocument object
#' @param param_list param names vector, used for recursive calls
#' @param full_list TRUE or getting all names, FALSE otherwise (default)
#' for unique names list
#'
#' @return a character vector of parameters names
#'
#' @export
get_params_names <- function(xml_object,param_list = c(), full_list = FALSE) {

  # TODO
  # - for all: add an input parameter for specifying which formalism to use
  # to restrict params names list !

  xml_node <- NULL
  param_name <- NULL

  # If xml_object converting input argument to an XML node
  if ( class(xml_object) =="xmlDocument" ) {
    xml_node <- xmlRoot(xml_object@content)
  } else if (class(xml_object) == "XMLInternalElementNode") {
    xml_node <- xml_object
  }

  # Raising an error on xml_node type
  if ( base::is.null(xml_node) ) {
    stop("The XML object is neither an xml node nor an xml document !")
  }

  node_name <- xmlName(xml_node)

  # getting only one usm node or sol node
  if ( node_name %in% c("sols","usms") ) {
  #if ( node_name == "usms" ) {
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

  # Getting param_name from a node name
  # but not in list "plante", "horizon" ,"initialisations", "sol"
  if ( attr_name == "none" &&
       ! ( node_name %in% c("plante", "horizon", "initialisations", "sol")) ) {
    param_name <- node_name
  }


  # Getting param_name from an attribute value
  if ( attr_name %in% names(xmlAttrs(xml_node))) {
    param_name <- xmlAttrs(xml_node)[attr_name]
  }


  # Adding the param name to the param names list
  # - if it does not exist
  # - if a full param names list is asked
  if ( ! base::is.null(param_name) &&
       (full_list || ! (param_name %in% param_list )) )  {
    param_list <- c(param_list, param_name)
  }


  # for a text node, not any childs
  if ( (!childs_nb ) || (length(childs_names) == 1 && childs_names == "text") ) {
    return(param_list)
  }


  # Loop over childs and recursive call to the function
  for (n in 1:childs_nb) {

    if ( ! class(childs[[n]]) == "XMLInternalElementNode") {
      #print(class(childs[[n]]))
      next
    }
    param_list <- get_params_names(childs[[n]], param_list, full_list = full_list)
  }

  names(param_list) <- NULL

  # filtering unwanted names
  names_filt <- c("ta_entete", "tableau_entete", "ta", "tableau", "choix" )
  param_list <- setdiff(param_list,names_filt)
  return(param_list)
}
