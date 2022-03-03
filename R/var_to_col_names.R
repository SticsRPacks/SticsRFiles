#' @title  Convert Stics variables names, or generated column names to
#' valid conventional column names.
#' @description Change Stics variables names as valid variable R name
#' Like `varname(n)` or `varname.n.` to  `varname_n`.
#' Other names are unchanged (i.e.: varname, varname_n, ...)
#' @param var_vec Valid names vector
#'
#' @return Vector of formatted column names
#'
#'
#' @examples
#' var_names <- c("var1", "var2(n)", "var2.n.")
#' valid_names <- SticsRFiles:::var_to_col_names(var_names)
#' @keywords internal
#'
var_to_col_names <- function(var_vec) {
  . <- NULL
  # Case: varname(n)
  var_vec <-
    gsub("\\(", "_", var_vec) %>%
    gsub("\\)", "", .)

  # Case: varname.n.
  var_vec <-
    gsub("\\.", "_", var_vec) %>%
    gsub("_$", "", .)

  return(var_vec)
}



#' Variable name to stics variable name
#'
#' @description Convert a variable name into a stics-compatible variable name. E.g.
#' `lai_n` into `lai(n)`.
#'
#' @param var     Vector of variable names
#' @param version Stics version.
#'
#' @return A vector of variable names compatible with stics (usually used for `gen_varmod()`)
#' @keywords internal
#'
#' @seealso `gen_varmod()`
#'
#' @examples
#' \dontrun{
#' var_to_stics_name(c("lai_n", "masec_n"))
#' }
#'
var_to_stics_name <- function(var, version = "latest") {
  var_exist <- is_stics_var(var, version)
  if (any(!var_exist)) {
    stop()
  }
  all_vars <- all_out_var(version)
  var_parsed <- var_to_col_names(var)
  vars_names_parsed <- var_to_col_names(all_vars$name)
  as.character(all_vars$name[match(var_parsed, vars_names_parsed)])
}
