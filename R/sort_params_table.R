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
sort_params_table <- function(params_table) {

  # Getting parameters names
  pattern <- "_[0-9]*$"
  par_names <- names(params_table)
  names_list <- unique(gsub(pattern = pattern,"",par_names))


  # Applying function to each sub table for each parameter name
  if ( length(names_list) > 1){
    out_table <- lapply( names_list,
                         function(x) sort_params_table(dplyr::select(params_table,dplyr::starts_with(x))))
    names(out_table) <- names_list
    return(out_table)
  }

  nb_col <- length(grep(pattern = pattern, par_names))

  # Parameter is a scalar one (not indexed with _N)
  if (! nb_col ) {
    return(params_table)
  }

  # Getting param name and suffix indices
  param_name <- gsub(pattern = pattern,"",par_names[1])
  pattern <- paste0("^",param_name,"_")
  # Sorting indices and param names indexed
  sorted_idx <- sort(as.numeric(gsub(pattern = pattern,"",par_names)))
  par_names <- paste0(param_name,"_", sorted_idx)

  # Selecting sorted data
  return(dplyr::select(params_table,par_names))
}
