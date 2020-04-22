
#' Return all possible STICS outputs for var.mod
#'
#' @description Helper function to print the list of all possible variables to set as output
#' from the STICS model.
#'
#' @param version The stics version. See `get_stics_versions_compat()` to get all compatible versions. Default
#' to "last", a special code to get the last version.
#'
#' @seealso `get_var_info()`, `gen_varmod()`, and `get_stics_versions_compat()`
#'
#' @examples
#' \dontrun{
#'  all_out_var()
#' }
#' @keywords internal
#'
all_out_var <- function(version= "last"){
  if(version=="last"){
    version <- get_stics_versions_compat()$last_version
  }
  version <- match.arg(version, get_stics_versions_compat()$versions_list, several.ok = FALSE)
  utils::read.csv2(system.file(file.path("extdata/csv",version,"outputs.csv"), package = "SticsRFiles"))
}


#' Find output variable names for STICS
#'
#' @description Helper function that return the name used as input
#' for the STICS model with a partial match.
#'
#' @param var Character vector with a (partial) STICS output variable name
#' @param keyword Search by keyword instead of the variable name (search in the description)
#' @param version The stics version. See `get_stics_versions_compat()` to get all compatible versions. Default
#' to "last", a special code to get the last version.
#'
#' @details The function understand \code{\link[base]{regex}} as input.
#'
#' @seealso \code{\link{all_out_var}}
#'
#' @examples
#' # Find by variable name (fuzzy search):
#' SticsRFiles::get_var_info("lai")
#'
#' # Find by keyword (fuzzy search in variable description):
#' SticsRFiles::get_var_info(keyword= "lai")
#'
#' # Find for a particular version:
#' SticsRFiles::get_var_info("lai", version= "V9.0")
#' @export
#'
get_var_info <- function(var=NULL,keyword=NULL,version= "last"){
  all_vars <- all_out_var(version)
  if(!is.null(var)){
    var= var_to_col_names(var)
    vars_names_parsed= var_to_col_names(all_vars$variable)
    all_vars[grep(var, vars_names_parsed,ignore.case = TRUE),]
  }else if(!is.null(keyword)){
    all_vars[grep(keyword, all_vars$details,ignore.case = TRUE),]
  }else{
    all_vars
  }
}

#' Search if a STICS variable exist
#'
#' @description Tells if one or more variable names are valid STICS output variables.
#'
#' @param var     A vector of variable names
#' @param version The version of the model
#'
#' @return A boolean vector: `TRUE` if the variable exist, `FALSE` if it doesn't
#'
#' @seealso `get_var_info()` for interactive use.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' is_stics_var(c("lai(n)",'masec(n)',"truc"))
#' }
is_stics_var= function(var,version= "last"){
  all_vars <- all_out_var(version)
  var_parsed= var_to_col_names(var)
  vars_names_parsed= var_to_col_names(all_vars$variable)

  index_var= match(var_parsed,vars_names_parsed)
  var_found= !is.na(index_var)
  if(any(!var_found)){
    cli::cli_alert_warning("Variable{?s} {.var {var_parsed[!var_found]}} not found. Try {.code get_var_info()}.")
  }
  return(var_found)
}



