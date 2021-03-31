#' @title Generate from a template or modify a Stics tec xmlDocument
#'
#' @param xml_doc an xmlDocument object (created from an ini file)
#' @param param_table a table (df, tibble) containing parameters to use
#' @param stics_version the stics files version to use (optional, default to last). Only used if xml_doc = NULL.
#' @param dict List of correspondance between given parameter names and Stics internal names.
#' @param ... Additional arguments
#'
#' @return an invisible xmlDocument object or a list of
#'
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' download_usm_xl(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' tec_param_df <- read_excel(xl_path, sheet = "Tec")
#' tec_doc <- SticsRFiles:::gen_tec_doc(param_table = tec_param_df)
#'
#' }
#'
#' @keywords internal
#'
gen_tec_doc <- function(xml_doc = NULL,
                        param_table = NULL,
                        stics_version = "last",
                        dict = NULL,
                        ...) {


  # Fix first time
  sort = TRUE
  dot_args= list(...)
  if ("sort" %in% names(dot_args)) sort = dot_args$sort

  # check/get version
  stics_version <- get_xml_stics_version(stics_version = stics_version,
                                         xml_doc = xml_doc)

  # getting a default xml template
  if ( base::is.null(xml_doc) ) {
    xml_doc <- get_xml_base_doc("tec",stics_version = stics_version)
  }

  # Nothing to do
  if ( base::is.null(param_table) ) {
    return(xml_doc)
  }

  # managing several doc generation based upon the lines number in param_table
  lines_nb <- dim(param_table)[1]
  if (lines_nb > 1) {
    xml_docs <- apply(param_table,1,
                      function(x) gen_tec_doc( xml_doc = cloneXmlDoc(xml_doc),
                                               param_table = as.data.frame(t(x),
                                                                           stringsAsFactors = F ),
                                               stics_version = stics_version,
                                               dict = dict,
                                               sort = FALSE))
    return(xml_docs)
  }

  gen_error <- FALSE

  # TODO : Avoid making conversion at each call !!!!!!
  # getting values of params declared in the table
  table_params <- get_params_from_table(params_table = param_table, xml_doc = xml_doc, dict = dict)
  table_names <- names(table_params)


  # checking parameters names
  # doc param names
  doc_params <- get_params_from_doc(xml_doc)

  # getting unknown param names
  unknown_param <- setdiff(unlist(table_names),unlist(doc_params))

  # temporary select for avoiding errors
  unknown_idx <- table_names %in% unknown_param

  # TODO: message listing unknown parameter names !!!

  # Updating param names and table
  if (any(unknown_idx)) {
    table_params <- table_params[ ! unknown_idx ]
    table_names <- names(table_params)
  }

  # Getting scalar and vector parameter names
  vec_idx <- unlist(lapply(table_params,
                           function(x) length(grep(pattern = "_[0-9]*$",names(x))) > 0 ), use.names = F)

  scal_names <- table_names[! vec_idx]
  vec_names <- table_names[vec_idx]

  # Setting scalar parameters values
  for ( scal_name in scal_names) {
    set_param_value(xml_doc, scal_name, table_params[[ scal_name ]]) #, show_xpath = T)
  }

  # Setting vector parameters
  # loop over vector names

  # Special case for cutting
  # treating first julfauche or tempfauche, bc other
  # sibling parameters have common names : hautcoupe,
  # lairesiduel, msresiduel, anitcoupe
  # otherwise parameters might be set in the wrong "choix" block !
  fauche <- intersect(c("tempfauche", "julfauche","hautcoupe",
                        "lairesiduel", "msresiduel","anitcoupe"),
                      vec_names)
  if (length(fauche)) vec_names <- c(fauche, vec_names[!vec_names %in% fauche])

  for (par_name in vec_names) {

    #print(par_name)

    nb_par <- get_param_number(xml_doc, par_name)
    nb_values <- length(table_params[[par_name]])

    # If the nodes number is already matching
    # or a previous pass in the else condition
    # added needed nodes number for the parameter or set of.
    if ( ! base::is.null(nb_values) && (nb_par == nb_values)) {
      set_param_value(xml_doc, param_name = par_name,
                      param_value = table_params[[par_name]])
    } else {

      if ( nb_par > 0 ) {
        # Remove all intervention nodes
        # in order to add new ones

        xpath_node <- get_param_type(xml_doc = xml_doc,
                                     param_name = par_name)$xpath

        # the parameter is a simple one as <param ...> in xml file
        # directly attached to a <formalisme...> parent, so the formalisme
        # node cannot be removed. Consistency error between the
        # values detected in the parameters table and the param type
        # in the xml file.
        if (xmlName(xmlParent(getNodeS(xml_doc,xpath_node)[[1]])) == "formalisme") {
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

        # Removing existing nodes, for creating new set of
        # after that
        remove_parent_from_doc(xml_doc = xml_doc,
                               param_name = par_name)

      }

      # Generating an "intervention" node containing par_name in colonne nom
      # attribute
      # Cloning "ta_entete" node, from the current xml_doc
      # renaming it and reusing it for intervention nodes creation
      op_node <- xmlClone(getNodeS(xml_doc,
                                   paste0("//ta_entete[colonne[@nom='",par_name,"']]"))[[1]])
      xmlName(op_node) <- "intervention"


      # Getting needed nodes number and formalism or choice
      # to which they are to be attached
      nodes_nb <- length(table_params[[par_name]])
      par_form <- get_param_formalisms( xml_doc = xml_doc, par_name)

      if ( base::is.null(par_form)) {
        print(paste("Error: formalism for:", par_name))
      }

      # formalism or specific choix path to be calculated
      idx <- c("tempfauche", "julfauche") %in% par_name
      choix <- c("calendar in degree days","calendar in days")
      if (any(idx)) {
        parent_path <- get_param_type(xml_doc,"ta","choix", choix[idx])
      } else {
        parent_path <- get_param_type(xml_doc,"ta","formalisme",par_form)
      }

      # adding needed nodes : nodes_nb
      add_node_to_doc(xml_doc = xml_doc,
                      new_node = op_node,
                      nodes_nb = nodes_nb,
                      parent_path = parent_path$xpath)

      # Setting values for all the concerned nodes
      set_param_value(xml_doc = xml_doc,
                      param_name = par_name ,
                      param_value = table_params[[par_name]])

      # Fixing nb_interventions
      # according to specificities linked to common parameter names
      # in choice "calendar in days" and "calendar in degree days"
      if (any(idx)) {
        # specific cas for "cut crop"
        set_param_value(xml_doc = xml_doc,"nb_interventions",nodes_nb,choix[idx])
      } else {
        # general case for operations
        set_param_value(xml_doc = xml_doc,"nb_interventions",nodes_nb,par_form)
      }
    }

  }

  # A PRIORI : Seulement pour ce qui concerne fauches !!!!!!!!!!!
  # avec nb_interventions == 0
  # TODO: remove useless intervention nodes, if any
  # Set in template files from the package
  # Nodes list for which day or temp sum == 999
  # SticsRFiles:::getNodeS(out_tec, "//intervention[colonne[1]=999]")

  if (gen_error) {
    xml_doc <- NULL
  }

  return(invisible(xml_doc))

}
