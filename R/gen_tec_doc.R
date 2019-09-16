#' @title Generate  from a template or modify a Stics tec xmlDocument
#' @param xml_doc_object an xmlDocument object (created from an ini file)
#'
#' @param param_table a table (df, tibble) containing parameters to use
#' @param stics_version the stics files version to use
#'
#' @return an xmlDocument object or a list of
#'
#' @export
#'
gen_tec_doc <- function(xml_doc_object = NULL,
                        param_table = NULL,
                        stics_version = "last",
                        dict = NULL) {

  # check/get version
  stics_version <- get_xml_stics_version(stics_version = stics_version,
                                         xml_doc = xml_doc_object)

  # getting a default xml template
  if ( is.null(xml_doc_object) ) {
    xml_doc_object <- get_xml_base_doc("tec",stics_version = stics_version)
  }

  # Nothing to do
  # if ( is.null(param_table) ) {
  #   return(xml_doc_object)
  # }


  # managing several doc generation based upon the lines number in param_table
  lines_nb <- dim(param_table)[1]
  if (lines_nb > 1) {
    #xml_docs <- vector("list",lines_nb)
    xml_docs <- apply(param_table,1,
                      function(x) gen_tec_doc(xml_doc_object = cloneXmlDoc(xml_doc_object),
                                              param_table = as.data.frame(t(x),
                                                                          stringsAsFactors = F ),
                                              stics_version = stics_version,
                                              dict = dict))
    return(xml_docs)
  }

  gen_error <- FALSE

  # TODO : Avoid making conversion at each call !!!!!!
  # getting values of params declared in the table
  table_params <- get_params_from_table(param_table,"tec",xml_doc_object, dict = dict)
  table_names <- names(table_params)


  # checking parameters names
  # doc param names
  doc_params <- get_params_from_doc(xml_doc_object)

  # getting unknown param names
  unknown_param <- setdiff(table_names,unlist(doc_params))

  # temporary select for avoiding errors
  table_params <- table_params[ table_names %in% unknown_param == FALSE ]
  table_names <- names(table_params)


  # getting scal names
  vec_idx <- unlist(lapply(table_params,
                           function(x) length(grep(pattern = "_[0-9]*$",names(x))) > 0 ), use.names = F)

  scal_names <- table_names[! vec_idx]
  vec_names <- table_names[vec_idx]


  # Setting scalar parameters values
  #
  for ( scal_name in scal_names) {
    set_param_value(xml_doc_object, scal_name, table_params[[ scal_name ]]) #, show_xpath = T)
  }


  # test for controlling values set
  #print(paste("value set for iplt0",get_param_value(xml_doc_object, "iplt0")))


  # Setting vector parameters
  # loop over vector names
  for (par_name in vec_names) {

    #print(par_name)

    nb_par <- get_param_number(xml_doc_object, par_name)
    nb_values <- dim(table_params[[par_name]])[2]
    if ( nb_par == nb_values) {
      # if the nodes number is already matching
      # or a previous pass in the else condition
      # permitted to add the needed nodes number.
      #print(par_name)

      #print(paste0("avant set : ",par_name))
      set_param_value(xml_doc_object,param_name = par_name,
                      param_value = table_params[[par_name]]) #, show_xpath = TRUE)
    } else {

      if ( nb_par > 0 ) {
        # Remove all intervention nodes for adding new ones

        xpath_node <- get_param_type(xml_doc = xml_doc_object,
                                     param_name = par_name)$xpath

        # the parameter is a simple one as <param ...> in xml file
        # directly attached to a <formalisme...> parent, so the formalisme
        # node cannot be removed. Consistency error between the
        # values detected in the parameters table and the param type
        # in the xml file.
        if (xmlName(xmlParent(getNodeS(xml_doc_object,xpath_node)[[1]])) == "formalisme") {
          gen_error <- TRUE
          cat(paste("The parameter",par_name,"is unique in the original xml file, and not attached to \"intervention\"\n"))
          cat("Multiple values are present in input table, check consistency with formalism definition !\n")
          cat("The treatment for this parameter is aborted.")
          cat("\n")
          next
        }

        if (par_name == "engrais") {
          print("ok for multiple engrais !")
        }
        remove_parent_from_doc(xml_doc = xml_doc_object,
                               param_name = par_name)


      }



      nodes_nb <- ncol(table_params[[par_name]])

      par_form <- get_param_formalism( xml_document = xml_doc_object, par_name)

      if ( is.null(par_form)) {
        print(paste("Error: formalism for par_name"))
      }

      # formalism to be detected in get_xml_base_node from param_name
      parent_path <- get_param_type(xml_doc_object,"ta","formalisme",par_form)
      op_node <- get_xml_base_node("tec",par_form)
      #
      #
      #
      # adding needed nodes : nodes_nb
      add_node_to_doc(xml_doc = xml_doc_object,new_node = op_node,
                      nodes_nb = nodes_nb,
                      parent_path = parent_path$xpath)

      # TODO : replace with, but not functionning
      # add_stics_nodes(xml_doc = xml_doc_object,
      #                 file_tag = "tec",
      #                 formalism_name = par_form,
      #                 stics_version = stics_version)

      #print(paste0("avant set : ",par_name))
      # Setting values for all the concerned nodes



      set_param_value(xml_doc_object = xml_doc_object,
                      param_name = par_name ,
                      param_value = table_params[[par_name]])

      # fixing nb interventions
      par_form <- get_param_formalism(xml_doc_object,par_name)
      set_param_value(xml_doc_object = xml_doc_object,"nb_interventions",nodes_nb,par_form)

      # if ( par_name == "julapI_or_sum_upvt" ) {
      #   browser()
      #   print(get_param_value(xml_doc_object,"julapI_or_sum_upvt"))
      # }

    }

  }


  if (gen_error) {
    xml_doc_object <- NULL
  }

  return(xml_doc_object)

}
