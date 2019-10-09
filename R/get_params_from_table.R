#' @title Get a list of Stics xml parameters names and values from a table (data.frame, tibble)
#' @param params_table a table (df, tibble) containing parameters to use
#'
#' @param param_names parameters names to select
#' @param xml_doc an xmlDocument object (of any xml type)
#' @param lines_id table lines identifiers to select
#' @param stopping logical value for stopping if any unknown parameters (if TRUE)
#' @param dict List of names correspondence between short names (tags) and real parameters names
#'
#' @return a named list (with param names as list names) of data.frame/tibble
#'
#' @export
get_params_from_table <- function(params_table,
                                  param_names = NULL,
                                  xml_doc = NULL,
                                  lines_id = NULL,
                                  stopping = FALSE,
                                  dict = NULL) {

  # TODO: doing a merge with get_values_from_table
  library(stringr)
  library(dplyr)



  if (is.null(dict)) {
    dict <- list(julapI="julapI_or_sum_upvt",doseI="amount",
                 julapN="julapN_or_sum_upvt",doseN="absolute_value/%")
  }

  # TODO : perhaps add checking of dict list content !!!

  if ( ! nargs() ) {
    return(dict)
  }


  # replacing column names starting with names of the dico list
  # with values of corresponding field in dico
  for (key in names(dict)) {
    params_table <- params_table %>%
      dplyr::rename_at(dplyr::vars(dplyr::starts_with(key)),
                       dplyr::funs(stringr::str_replace(.,key,dict[[key]])))
  }



  # getting values from table
  param_values <- get_values_from_table(params_table, param_names = NULL, lines_id = NULL)


  # checking if all params from xl table exist in xml doc file
  tbl_par_names <- names(param_values)
  doc_par_names <- unlist(get_params_from_doc(xml_doc), use.names = F)

  unknown_params <- setdiff(tbl_par_names, doc_par_names)

  if ( length(unknown_params) ) {
    message_str <- sprintf("\n%s\n%s\n\n%s\n\n%s\n\n","Unknown parameters found in table: ",
                           paste(unknown_params, collapse = ", "),
                           "Verify parameters names in xml files or",
                           "check substitution names in list returned by get_param_from_table()")
    if ( stopping) {
      stop(message_str)
    } else {
      cat(message_str)
    }

    # removing unknown param columns
    param_values[! names(param_values) %in% unknown_params]
  }

  return(param_values)


}