#' Add node to a Stics XML document
#'
#' @param xml_doc XML document
#' @param file_tag File tag (i.e. usms, sols, or tec)
#' @param nodes_nb Nodes numbers
#' @param formalism_name Name of the formalism
#' @param stics_version The version of STICS (eg "V9.0")
#'
#@export
#'
add_stics_nodes <- function(xml_doc, file_tag, nodes_nb = 1,formalism_name = NULL,
                            stics_version = "last" ) {
  # getting xml doc types == root node name
  # i.e. : "initialisation", "usms",...
  doc_types <- is_stics_doc()
  node_types <- get_xml_base_node()



  # checking if the tag corresponds to the file content
  doc_ok <- switch(file_tag,
                   usms = is_stics_usms(xml_doc),
                   sols = is_stics_sols(xml_doc),
                   tec = is_stics_tec(xml_doc))

  if (base::is.null(doc_ok)) { doc_ok <- FALSE }

  if ( ! doc_ok ) {
    stop("The xml document is of wrong type")
  }

  file_idx <- which(node_types$files_tags == file_tag)
  if ( ! length(file_idx) ) {
    stop(paste0("Unknown file tag :",file_tag))
  }

  parent_name <- node_types$parent[file_idx]
  node_name <- node_types$node_names[file_idx]

  form_idx <- which(node_types$form_names[[file_tag]] == formalism_name)

  add_form = FALSE
  # checking formalisms tag
  if ( ! base::is.null(formalism_name) && length(form_idx) ) {
    add_form = TRUE
    #formalism_name <- node_types$form_names[[file_tag]][form_idx]
    formalism_tag <- node_types$form_tags[[file_tag]][form_idx]

    if (formalism_tag %in% c("cutJul", "cutTemp")) {
      stop("special techniques cutting not implemented !")
    }
  }

  if ( add_form ) {
    # case : residus, tillage, irrigation, ferti,
    # TODO : special techniques cutting ???
    parent_path = paste0("//formalisme[@nom=\"",
                         formalism_name,"\"]/",
                         parent_name)
    # TODO: OR  ?
    #parent_path <- get_param_type(xml_doc,parent_name,
    #"formalisme",formalism_name)
  } else {
    # case usms, sols
    parent_path = paste0("//",parent_name)
  }

  # getting the new node to add
  new_node <- get_xml_base_node(file_tag = file_tag,
                              form_name = formalism_name,
                            stics_version = stics_version)

  # adding new node
  #addNodes(xml_doc, nodes_to_add = new_node, parent_path = parent_path)
  add_node_to_doc(xml_doc, new_node, nodes_nb = nodes_nb, parent_path = parent_path)

  # updating if needed interventions_nb
  if (XML::xmlName(new_node) =="intervention") {
    nb_interventions <-
      as.numeric(getAttrsValues(xml_doc, parent_path,"nb_interventions")) + 1
    setAttrValues(xml_doc,parent_path, "nb_interventions", nb_interventions)
  }


}
