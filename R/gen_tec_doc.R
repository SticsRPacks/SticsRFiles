#' @title Generate from a template or modify a STICS tec xml_document
#'
#' @param xml_doc an xml_document object (created from a technical file)
#' @param param_table a table (df, tibble) containing parameters to use
#' @param stics_version the STICS files version to use (optional,
#' default to latest). Only used if xml_doc = NULL.
#' @param dict List of correspondence between given parameter names and
#' STICS internal names.
#' @param ... Additional arguments (for example, coming from a call
#' from gen_tec_xml using a na_values argument)
#'
#' @return an invisible xml_document object or a list of
#'
#' @importFrom tidyselect where
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' download_usm_xl(
#'   file = "inputs_stics_example.xlsx",
#'   dest_dir = "/path/to/dest/dir"
#' )
#' xl_path <- file.path("/path/to/dest/dir", "inputs_stics_example.xlsx")
#' tec_param_df <- read_excel(xl_path, sheet = "Tec")
#' tec_doc <- gen_tec_doc(param_table = tec_param_df)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
gen_tec_doc <- function(
  xml_doc = NULL,
  param_table = NULL,
  stics_version = "latest",
  dict = NULL,
  ...
) {
  dot_args <- list(...)
  dot_args_names <- names(dot_args)

  na_values <- NA
  if ("na_values" %in% dot_args_names) na_values <- dot_args$na_values

  # Getting version
  stics_version <- get_xml_stics_version(
    stics_version = stics_version,
    xml_doc = xml_doc
  )

  # Getting a default xml template according to Stics version
  if (base::is.null(xml_doc)) {
    xml_doc <- get_xml_base_doc("tec", stics_version = stics_version)
  }

  # Nothing to do, returning only an xml_doc from a template or a given file
  if (base::is.null(param_table)) {
    return(xml_doc)
  }

  # Managing several doc generation based upon lines in param_table
  lines_nb <- dim(param_table)[1]
  if (lines_nb > 1) {
    xml_docs <- apply(
      param_table,
      1,
      function(x) {
        gen_tec_doc(
          xml_doc = clone_xml_doc(xml_doc),
          param_table = as.data.frame(t(x), stringsAsFactors = FALSE),
          stics_version = stics_version,
          dict = dict,
          na_values = na_values
        )
      }
    )
    return(xml_docs)
  }

  gen_error <- FALSE

  # TODO : Avoid loading the table in each call ?
  param_table <- param_table %>%
    dplyr::select(
      where(function(x) !is.na(x)) &
        where(function(x) {
          c <- x != "NA"
          if (is.na(c)) c <- TRUE
          return(c)
        })
    )

  # Getting values of parameters declared in the table
  table_params <- get_params_from_table(
    params_table = param_table,
    xml_doc = xml_doc,
    dict = dict
  )

  table_names <- names(table_params)

  # Checking parameters names
  # doc param names
  doc_params <- get_params_from_doc(xml_doc)

  # Getting unknown param names
  unknown_param <- setdiff(unlist(table_names), unlist(doc_params))

  # temporary select for avoiding errors
  unknown_idx <- table_names %in% unknown_param

  # Updating param names and table
  if (any(unknown_idx)) {
    # Message for unknown parameters
    warning(
      paste(table_params[unknown_idx], collapse = ", "),
      ": unknown parameters, they were removed !"
    )
    table_params <- table_params[!unknown_idx]
    table_names <- names(table_params)
  }

  # Getting scalar parameters list and vector parameters list
  vec_idx <- unlist(
    lapply(
      table_params,
      function(x) length(grep(pattern = "_[0-9]*$", names(x))) > 0
    ),
    use.names = FALSE
  )

  scal_names <- table_names[!vec_idx]
  vec_names <- table_names[vec_idx]

  # Setting scalar parameters values
  for (scal_name in scal_names) {
    set_param_value(xml_doc, scal_name, table_params[[scal_name]])
  }

  # Setting vector parameters
  #
  # Special case for cutting:
  # treating first julfauche or tempfauche, bc other
  # sibling parameters have common names : hautcoupe,
  # lairesiduel, msresiduel, anitcoupe
  #
  # Choice between tempfauche and julfauche
  # 1-Checking if "codemodfauche" exists in table_params and that
  # one of the choices is used!
  # 2- Getting the right mode fauche parameter name
  #

  # checking if there is either tempfauche or
  # julfauche, but not both !
  if ("codemodfauche" %in% table_names) {
    code <- table_params[["codemodfauche"]]

    if (
      any(
        grepl("^tempfauche", table_names)
      ) &&
        any(grepl("^julfauche", table_names))
    ) {
      # Finalizing the xml doc destruction to avoid errors
      delete(xml_doc)
      xml_doc <- NULL
      # both parameters exist, stopping whatever the codemodfauche value is
      stop(
        "The parameters 'tempfauche' and 'julfauche' cannot be used at the same time.",
        "See what is expected according to 'codemodfauche'. Fix the table content."
      )
    }
  }

  # Case of thinning: removing parameters
  # if codeclaircie is set to 1, no thinning
  if ("codeclaircie" %in% table_names) {
    if (table_params[["codeclaircie"]] == "1") {
      vec_names <- setdiff(vec_names, c("juleclair", "nbinfloecl"))
    }
  }

  # Flag to fix if the intervention number has to be written
  # will be set to FALSE, whenthe first cutting parameter is detected
  write_cut_nb <- TRUE

  for (par_name in vec_names) {
    nb_par <- get_param_number(xml_doc, par_name)
    param_values <- table_params[[par_name]]

    # If the nodes number is already matching
    # or a previous pass in the else condition
    # added needed nodes number for the parameter or set of.
    #
    nb_values <- length(param_values)
    if (nb_values == 0) next
    if ((nb_values > 0) && nb_par == nb_values) {
      set_param_value(
        xml_doc,
        param_name = par_name,
        param_value = param_values
      )
    } else {
      if (nb_par > 0) {
        # Removing all existing intervention nodes
        # in order to add new ones and set parameters values
        xpath_node <- get_param_type(
          xml_doc = xml_doc,
          param_name = par_name
        )$xpath

        # The parameter is a simple one as <param ...> in xml file
        # directly attached to a <formalisme...> parent, so the formalisme
        # node cannot be removed. Consistency error between the
        # values detected in the parameters table and the param type
        # in the xml file.
        if (
          XML::xmlName(XML::xmlParent(get_nodes(
            xml_doc,
            xpath_node
          )[[1]])) ==
            "formalisme"
        ) {
          gen_error <- TRUE
          message(paste(
            "The parameter",
            par_name,
            "is unique in the original xml file,",
            "and not attached to \"intervention\"\n"
          ))
          message(paste0(
            "Multiple values are present in input table,",
            " check consistency with formalism definition !\n"
          ))
          message("The treatment for this parameter has aborted.\n")
          next
        }

        # Removing existing nodes, for creating new set of
        # after that
        remove_parent_from_doc(
          xml_doc = xml_doc,
          param_name = par_name
        )
      }

      # Generating an "intervention" node containing par_name in colonne 'nom'
      # attribute
      # Cloning "ta_entete" node, from the current xml_doc
      # renaming it and reusing it for intervention nodes creation
      op_node <- XML::xmlClone(get_nodes(
        xml_doc,
        paste0("//ta_entete[colonne[@nom='", par_name, "']]")
      )[[1]])
      XML::xmlName(op_node) <- "intervention"

      # Getting needed nodes number and formalism or choice
      # to which they are to be attached
      # nodes_nb <- length(param_values)
      par_form <- get_param_formalisms(xml_doc = xml_doc, par_name)

      if (base::is.null(par_form)) {
        message(paste("Error: formalism for:", par_name))
      }

      # 1 - General case, linked to formalisms
      parent_path <- get_param_type(xml_doc, "ta", "formalisme", par_form)$xpath
      parent_name <- par_form

      #-------------------------------------------------------------------------
      # 2 - Specific cases linked to options/choix
      # for getting parent path of intervention nodes to create
      #
      # Specific "choix" path to be calculated for cutting techniques
      cut_idx <- c("tempfauche", "julfauche") %in% par_name
      if (any(cut_idx)) {
        choix <- c("calendar in degree days", "calendar in days")[cut_idx]
        parent_path <- paste0(
          "//option[@nom='Method.of.cutting']/choix[@nom='",
          choix,
          "']/ta"
        )
        # replacing parent name
        parent_name <- choix
      }
      # Specific "option" calculation for thinning
      if (any(c("juleclair", "nbinfloecl") %in% par_name)) {
        parent_path <- "//option[@nom='thinning']/choix[@nom='yes']/ta"
      }
      #-------------------------------------------------------------------------

      # Adding needed nodes : nodes_nb
      add_node_to_doc(
        xml_doc = xml_doc,
        new_node = op_node,
        nodes_nb = length(param_values),
        parent_path = parent_path
      )

      # Setting values for all the concerned intervention nodes
      set_param_value(
        xml_doc = xml_doc,
        param_name = par_name,
        param_value = param_values
      )

      # Finally fixing nb_interventions
      if (!any(cut_idx) || write_cut_nb) {
        set_param_value(
          xml_doc = xml_doc,
          param_name = "nb_interventions",
          param_value = length(param_values),
          parent_name = parent_name
        )
      }
      # When a cutting parameter is detected for the first time
      if (any(cut_idx)) write_cut_nb <- FALSE
    }
  }

  if (gen_error) {
    delete(xml_doc)
    xml_doc <- NULL
  }

  return(invisible(xml_doc))
}
