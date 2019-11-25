#' @title Get a list of Stics xml parameters values from a table (data.frame, tibble)
#' @param params_table a table (df, tibble) containing parameters to use
#'
#' @param param_names parameters names
#' @param lines_id lines ids for selecting
#'
#' @return a named list (with param names as list names) of data.frame/tibble
#'
#' @export
get_values_from_table <- function(params_table, param_names = NULL, lines_id = NULL) {


  # TODO: doing a merge with get_params_from_table
  # calling get_values_from_table
  # working on a parameters names list

  tb_dims <- dim(params_table)

  select_lines <- ! is.null(lines_id)

  if ( select_lines && length(unique(lines_id)) > tb_dims[2] )  {
    stop("Index out of range for selecting line in table")
  }

  if (is.null(param_names)) {
    # getting param names list from table
    #param_names <- unique(gsub(pattern = "_[0-9]*$","",names(params_table)))
    param_names <- names(params_table)
  }

  patt <- "_[0-9]*$"
  # cut parameters in sub-groups for getting vectors
  scal_names <- grep(pattern = patt,param_names, invert = T, value = T)

  vec_names <- unique( gsub(pattern = patt, replacement = "",
                            x = grep(pattern = patt,param_names, value = T)) )


  splitted_params_vec <- lapply(vec_names,
                                function(x) dplyr::select(params_table,
                                                          dplyr::starts_with(paste0(x,"_"))))
  names(splitted_params_vec) <- vec_names

  # sort tables with param index suffix _X
  splitted_params_vec <- lapply(splitted_params_vec, sort_params_table)

  splitted_params_scal <- lapply(scal_names,function(x) dplyr::select(params_table,x))

  names(splitted_params_scal) <- scal_names

  splitted_params <- c(splitted_params_scal, splitted_params_vec)

  # selecting lines if lines_id
  if (select_lines) {
    splitted_params <- lapply(splitted_params,function(x) dplyr::slice(x,lines_id))
  }

  return(splitted_params)


}
