#' @title Get a list of Stics xml parameters names an xmlDocument or  XML node
#' @param xml_object an xml XMLInternalElementNode or xmlDocument object
#' @param param_list param names vector, only used for recursive calls
#' @param full_list TRUE for getting all names, FALSE otherwise (default)
#' @param root_name Only for getting the root node name (file type),
#' usefull for filtering unwanted names to be includes in parameters names list
#'
#' for unique names list
#'
#' @return a character vector of parameters names
#'
#' @examples
#' \dontrun{
#' xml_path <- file.path(get_examples_path( file_type = "xml"),"sols.xml")
#' sols_doc <- SticsRFiles:::xmldocument(xml_path)
#' SticsRFiles:::get_param_names(sols_doc)
#' }
#'
#' @keywords internal
#'
get_param_names <- function(xml_object,
                            param_list = c(),
                            full_list = FALSE,
                            root_name = NULL) {

  # TODO
  # - for all: add an input parameter for specifying which formalism to use
  # to restrict params names list !

  xml_node <- NULL
  param_name <- NULL

  # If xml_object converting input argument to an XML node
  if ( base::is.element("xmlDocument", class(xml_object)) ) {
    xml_node <- xmlRoot(xml_object@content)
    root_name <- xmlName(xml_node)
  } else if (base::is.element("XMLInternalElementNode", class(xml_object)) ) {
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
    xml_node <- xmlChildren(xml_node)[[1]]
  }

  # # filtering root nodes
  # if (node_name %in% c("fichierpar", "fichierparamgen", "fichiertec", "fichiersta",
  #                      "initialisations", "fichierplt")) {
  #   childs <- xmlChildren(xml_node)
  # }

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

  if (node_name == "tv") {
    attr_name = "nom"
  }

  if (node_name == "optionv") {
    attr_name = "nom"
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
  # but not in list "plante", "horizon" ,"initialisations", "sol", "usm"
  if ( attr_name == "none" &&
       ! ( node_name %in% c("plante", "horizon", "initialisations", "sol", "usm", "snow")) ) {
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

    if ( ! base::is.element("XMLInternalElementNode", class(childs[[n]])) ) {
      #print(class(childs[[n]]))
      next
    }
    param_list <- get_param_names(childs[[n]],
                                  param_list,
                                  full_list = full_list,
                                  root_name = root_name)
  }

  names(param_list) <- NULL

  # Filtering unwanted list elements using names
  names_filt <- c("ta_entete", "tableau_entete", "ta", "tableau", "choix",
                  "fichierpar", "fichierparamgen", "fichiertec", "fichiersta",
                  "initialisations", "fichierplt", "formalisme", "intervention",
                  "colonne", "formalismev")

  # Specific to plt file: variete also exists in fichierstec as a parameter
  if ( root_name == "fichierplt") names_filt <- c(names_filt, "variete")

  param_list <- setdiff(param_list,names_filt)

  return(param_list)
}
