#' @title  Convert columns names list to Stics variables names
#' @description Change the `varname.n.` or `varname_n` by `varname(n)`
#' other names are unchanged (i.e.: varname, varname(n), ...)
#' @param var_list Columns names list
#' @return List of Stics variables names
#@export
#'
#' @keywords internal
#'
col_names_to_var <- function(var_list=c()) {
  .= NULL

  # for varname_n
  var_list <-  gsub("_","(",var_list)

  any_opening <- grepl("\\(", var_list)

  # Closing ) only if ( is present
  var_list[any_opening] <- lapply(var_list[any_opening], function(x) {
    if ( !grepl(pattern = "\\)$", x)) x <- paste0(x,")")
    return(x)
  })


  # for varname.n.
  var_list <-
    gsub("\\.$",")",var_list)%>%
    gsub("\\.","(",.)

  return(var_list)

}
