#' @title Get a list of Stics xml parameters names from nodes attributes in an xmlDocument
#' @param xml_doc_object an xmlDocument object (created from an xml file)
#'
#' @param type_name type name, one of "option", "param", "colonne"
#' @param unique_val logical, TRUE to get unique names list, FALSE otherwise
#'
#' @return a named list of parameter names
#'
#' @export
get_params_from_doc_attr <- function(xml_doc_object, type_name = NULL, unique_val = TRUE) {

  # For tec, param newform, param gen, sols, station
  # files
  type_names <- c("option", "param", "colonne","colonne")
  name_fields <- c("nomParam", "nom", "nom", "nom")
  parent_names <- c("","","ta_entete/","tableau_entete/")
  #parent_for_values <- c("","","ta/")


  if (is.null(type_name)) {
    type_name <- type_names
  }

  type_id <- type_names %in% type_name

  if (! any(type_id) ) {
    stop(paste("Unknown param type_name : ",type_name))
  }

  name_field <- name_fields[type_id]
  parent_name <- parent_names[type_id]
  #parent_for_value <- parent_for_values[type_id]

  xpath <- paste0("//",parent_name,type_name)
  #xpath_values <- paste0("//",parent_for_value,type_name)

  nb_types <- length(type_name)

  params <- vector("list",nb_types)
  #values <- vector("list",nb_types)
  is_null <- rep(TRUE,nb_types)


  for (t in 1:nb_types) {
    tmp <- getAttrsValues(xml_doc_object, xpath[t], name_field[t])
    if ( !is.null(tmp) ) {
      params[[t]] <- tmp
      is_null[t] <- FALSE
    }
  }

  #print(params)

  params <- params[! is_null]
  names(params) <- type_name[! is_null]

  if (unique_val) {
    params <- lapply(params, unique)
  }

  return(params)

}