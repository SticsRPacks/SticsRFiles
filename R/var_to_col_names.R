#' @title  Convert Stics variables names, or generated column names to
#' Stics specific columns names
#' @description Change the `varname(n)` or `varname.n.` to `varname_n`
#' other names are unchanged (i.e.: varname, varname_n, ...)
#' @param var_list Variables names
#' @return List of formatted column names
#' @export
#'
var_to_col_names <- function(var_list=c()) {

  # varname(n)
  var_list <-
    gsub("\\(","_",var_list) %>%
    gsub("\\)","",.)

  # varname.n.
  var_list <-
    gsub("\\.","_",var_list) %>%
    gsub("_$","",.)

  return(var_list)

}
