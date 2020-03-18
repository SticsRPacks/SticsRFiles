#' @title Get a list of Stics xml parameters values from a table (data.frame, tibble)
#' @param params_table a table (df, tibble) containing parameters to use
#'
#' @param param_names parameters names
#' @param lines_id lines ids for selecting
#'
#' @return a named list (with param names as list names) of data.frame/tibble
#'
#' @examples
#' \dontrun{
#' copy_mailing_example(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' sols_param_df <- read_excel(xl_path, sheet = "Soils")
#'
#' SticsRFiles:::get_values_from_table(sols_param_df)
#'
#' SticsRFiles:::get_values_from_table(sols_param_df, param_names = c("DAF","HMINF"))
#'
#' SticsRFiles:::get_values_from_table(sols_param_df, lines_id = c(1,10))
#'
#' SticsRFiles:::get_values_from_table(sols_param_df,
#' param_names = c("DAF","HMINF"), lines_id = c(1,10))
#'
#' }
#'
#' @keywords internal
#'
get_values_from_table <- function(params_table, param_names = NULL, lines_id = NULL) {


  # TODO: doing a merge with get_params_from_table
  # calling get_values_from_table
  # working on a parameters names list

  tb_dims <- dim(params_table)

  select_lines <- ! base::is.null(lines_id)

  if ( select_lines && length(unique(lines_id)) > tb_dims[2] )  {
    stop("Index out of range for selecting line in table")
  }

  # Splitting parameters table into tables named list
  # with indexed parameters sorted (*_N)
  splitted_params <- split_params_table(params_table)

  # Selecting parameters tables with param_names
  if (!is.null(param_names)) {

    tables_idx <- names(splitted_params) %in% param_names

    if ( sum(tables_idx) != length(param_names)) {
      warning("Some of param_names do not exist in tables !")
    }

    splitted_params <- splitted_params[tables_idx]
  }

  # Selecting lines if lines_id
  if (select_lines) {
    splitted_params <- lapply(splitted_params,function(x) dplyr::slice(x,lines_id))
  }

  return(splitted_params)


}
