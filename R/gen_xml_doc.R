#' @title Generate from a template or modify a Stics sols or usms xmlDocument
#' @export
gen_xml_doc <- function(doc_type, xml_doc = NULL,
                        nodes_nb = NULL, nodes_param = NULL,
                        stics_version = "last", overwrite = F) {


  # for usms and sols files


  # check/get version of templates xml files
  stics_version <- get_xml_stics_version(stics_version = stics_version)


  doc_types <- list()
  doc_types$usms <- list(root="usms",node="usm")
  doc_types$sols <- list(root="sols",node="sol")

  if ( ! nargs()) {
    return(names(doc_types))
  }

  if ( ! is.element(doc_type, names(doc_types)) ) {
    stop(paste0("The doc type is not an existing one: ",doc_type))
  }

  root <- doc_types[[doc_type]]$root
  node <- doc_types[[doc_type]]$node

  root_str <- paste0("/",root)
  node_str <- paste0("//",node)

  # getting a default xml template
  if ( is.null(xml_doc) ) {
    # tmpl_file <- paste0("extdata/xml/templates/",stics_version,"/one_",root,".xml")
    # xml_doc <- xmldocument(system.file(tmpl_file, package = "SticsRFiles"))
    # using function get_xml_base_doc
    xml_doc <- get_xml_base_doc(xml_type = doc_type,
                                 stics_version = stics_version)
    overwrite = T
  }

  elts_nb <- NULL

  # identity or single usms/sols doc from the template file
  if ( all(is.null(c(nodes_nb, nodes_param))) ) {
    return(xml_doc)
  }

  # calculating nodes number
  if (! is.null(nodes_nb)) { elts_nb = nodes_nb }

  if ( "data.frame" %in% class(nodes_param) ) {
    elts_nb <- dim(nodes_param)[1]
  }

  # checking nodes number value
  if (is.null(elts_nb)) {
    stop("Error in usm number or in soils parameters table !")
  }

  # getting usm/sol nodes
  xml_nodes <- getNodeS(xml_doc, node_str)

  # Nothing to do
  if ( length(xml_nodes) == elts_nb && is.null(nodes_param) ) {
    return(xml_doc)
  }

  # Creating doc structure from a base node
  if ( overwrite ) {

    # Keeping only the root usms/sols node in the
    # xml document
    removeNodes(xml_nodes)

    # # Keeping the usm node as model, deconnected from the xml document
    # base_node_txt <- saveXML(xml_nodes[[1]])
    # # Adding usm nodes to usms node
    # new_node <- getNodeSet(xmlParse(base_node_txt), node_str)[[1]]

    new_node <- get_xml_base_node(root)

    #add_node_to_doc(xml_doc, new_node, nodes_nb = elts_nb, root_str )
    add_stics_nodes(xml_doc = xml_doc, file_tag = root, nodes_nb = elts_nb)

    # for ( u in 1:elts_nb ) {
    #   addNodes(xml_doc,new_node,root_str)
    # }

  }

  if ( is.null(nodes_param) ) {
    return(xml_doc)
  }

  # setting data according to doc_type
  switch( doc_type,
          usms = set_usms_param(xml_doc, nodes_param, overwrite = overwrite),
          sols = set_sols_param(xml_doc, nodes_param, overwrite = overwrite)
          )



  return(xml_doc)

}
