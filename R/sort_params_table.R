#' @title Sort a table against column names indexes with a suffix
#' @param params_table a table (df, tibble) with parameters names
#' indexed with suffixes _1, _2 ... i.e. paramname_1, paramname_2
#'
#' @export
sort_params_table <- function(params_table) {

  pattern <- "_[0-9]*$"
  par_names <- names(params_table)
  nb_col <- length(grep(pattern = pattern, par_names))

  if (! nb_col ) {
    # pattern not found : parameter is scalar
    return(params_table)
  }

  if ( ! nb_col == dim(params_table)[2] ) {
    #stop("Not unique parameter name found in table !")
    print(par_names)
    print("Not unique parameter name found in table !")
  }

  param_name <- gsub(pattern = pattern,"",par_names[1])

  pattern <- paste0("^",param_name,"_")
  sorted_idx <- sort(as.numeric(gsub(pattern = pattern,"",par_names)))

  par_names <- paste0(param_name,"_", sorted_idx)

  return(select(params_table,par_names))
}
