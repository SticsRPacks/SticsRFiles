#' @title  Convert Stics variables names, or generated column names to
#' valid conventional column names.
#' @description Change Stics variables names as valid variable R name
#' Like `varname(n)` or `varname.n.` to  `varname_n`.
#' Other names are unchanged (i.e.: varname, varname_n, ...)
#' @param var_vec Valid names vector
#' @return Vector of formatted column names
#@export
#'
#' @examples
#' var_names <- c("var1","var2(n)")
#' valid_names <- var_to_col_names(var_names)
#'
var_to_col_names <- function(var_vec) {
  .= NULL
  # Case: varname(n)
  var_vec <-
    gsub("\\(","_",var_vec) %>%
    gsub("\\)","",.)

  # Case: varname.n.
  var_vec <-
    gsub("\\.","_",var_vec) %>%
    gsub("_$","",.)

  return(var_vec)

}
