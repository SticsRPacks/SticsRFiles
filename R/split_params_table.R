#' @title Sort a table against column names indexes with a suffix
#' @param params_table a table (df, tibble) with parameters names
#' indexed with suffixes _1, _2 ... i.e. paramname_1, paramname_2
#'
#' @return Sorted table on indexed parameters names or a list of
#'
#' @examples
#' \dontrun{
#' copy_mailing_example(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' sols_param_df <- read_excel(xl_path, sheet = "Soils")
#'
#' SticsRFiles:::sort_params_table(sols_param_df)
#'
#' }
#'
#' @keywords internal
#'
split_params_table <- function(params_table) {

  # Getting parameters names
  # pattern <- "_[0-9]*$"
  # par_names <- names(params_table)
  # names_list <- gsub(pattern = pattern,"",par_names)
  # names_list <- unique(names_list)

  table_names <- names(params_table)

  # Sorting columns by name
  params_table <- params_table[ , order(table_names)]

  # Getting parameters names, without indes (i.e. _X)
  par_names <- unique(gsub(pattern = "_[0-9]*$", "",x = table_names))

  by_param_table <- get_by_param_table(params_table, par_names)

  return(by_param_table)


  # Applying function to each sub table for each parameter name
  #if ( length(names_list) > 1){
    # out_table <- lapply( names_list,
    #                      function(x) sort_params_table(dplyr::select(params_table,dplyr::starts_with(paste(x)))))

    # out_table <- lapply( names_list,
    #                      function(x) get_by_param_table(params_table, x))
    # names(out_table) <- names_list

   #     return(out_table)
  #}

  # nb_col <- length(grep(pattern = pattern, par_names))
  #
  # # Parameter is a scalar one (not indexed with _N)
  # if (nb_col < 2) {
  #   return(params_table)
  # }
  #
  # # Getting param name and suffix indices
  # param_name <- gsub(pattern = pattern,"",par_names[1])
  # pattern <- paste0("^",param_name,"_")
  # # Sorting indices and param names indexed
  # sorted_idx <- sort(as.numeric(gsub(pattern = pattern,"",par_names)))
  # par_names <- paste0(param_name,"_", sorted_idx)
  #
  # # Selecting sorted data
  # return(dplyr::select(params_table,par_names))
}


get_by_param_table <- function(params_table, param_name) {

  if (length(param_name) > 1) {
    out_table <- lapply( param_name,
                         function(x) get_by_param_table(params_table, x))
    #names(out_table) <- param_name
    out_table <- unlist(out_table, recursive = FALSE)
    return(out_table)
  }

  par_pattern <- paste0("(^",param_name,"$|","^",param_name,"_)")
  table_names <- names(params_table)
  par_names <-  grep(pattern = par_pattern, x = table_names, value = TRUE)
  par_idx <- table_names %in% par_names

  par_table <- list(params_table[ , par_idx])
  names(par_table) <- param_name
  return(par_table)

}
