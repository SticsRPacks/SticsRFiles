#' @title Extract a named list of parameters values or a list of
#' from data.frame row(s), for all parameters columns or a selection
#' @param params_table a table (df, tibble) with parameters names
#' indexed with suffixes _1, _2 ... i.e. paramname_1, paramname_2
#' or not for scalar parameters
#' @param param_name Optional arameter(s) name(s) vector
#' @param lines_id Optional data.frame lines numerical identifiers
#'
#' @return a named list of parameters values, with sorted values for multiple
#' parameter values (indexed with suffixes _1, _2, ...)
#'
#' @examples
#' \dontrun{
#' download_usm_xl(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' tec_param_df <- read_excel(xl_path, sheet = "Tec")
#' SticsRFiles:::get_values_by_param(params_table = tec_param_df)
#'
#' }
#'
#' @keywords internal
#'
get_values_by_param <- function(params_table, param_name = NULL, lines_id = NULL) {

  if ((base::is.null(param_name))) {
    param_name <- unique(gsub(pattern = "_[0-9]*$", "",x = names(params_table)))
  }

  # for multiple lines
  if (nrow(params_table) > 1) {
    # Lines selection
    if (!is.null(lines_id) &&
        base:is.numeric(lines_id)
        && max(lines_id) <= nrow(params_table)) {
      params_table <- params_table[lines_id,]
    }

    out_list <- apply(params_table, 1,
                      function(x) SticsRFiles ::: get_values_by_param(as.data.frame(t(x), stringsAsFactors = F ),
                                                                      param_name = param_name))
    return(out_list)
  }

  if (length(param_name) > 1) {
    out_table <- lapply( param_name,
                         function(x) get_values_by_param(params_table, x))
    #names(out_table) <- param_name
    out_table <- unlist(out_table, recursive = FALSE)
    return(out_table)
  }

  par_pattern <- paste0("(^",param_name,"$|","^",param_name,"_)")
  param_names <- grep(pattern = par_pattern, x = names(params_table), value = TRUE)

  # Sorting parameters names against a numeric suffix, if any
  if (length(grep(pattern = "_[0-9]", x = param_names))) {
    sorted_par_id <- sort(as.numeric(gsub(pattern = paste0(param_name,"_([0-9]+)"), replacement = "\\1", x = param_names)))
    param_names <- unlist(lapply(as.character(sorted_par_id), function(x) paste(param_name,x,sep = "_")))
  }

  # par_values <- suppressWarnings(as.numeric(params_table[,param_names]))
  # converting from data.frame to vector
  par_values <- unlist(params_table[,param_names])
  names(par_values) <- param_names
  par_table <- list(par_values)
  names(par_table) <- param_name
  return(par_table)

}
