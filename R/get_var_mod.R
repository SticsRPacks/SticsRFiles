
#' Get desired STICS outputs
#'
#' @description Get the variables STICS will return ("var.mod" file)
#'
#' @param workspace Path of the directory containing the Stics var.mod file to modify
#' @param usms_file Path (including name) of a USM XML file.
#' @param file_name `r lifecycle::badge("deprecated")` `file_name` is no
#'   longer supported, use `usms_file` instead.

#'
#' @return The variables that will be returned by STICS
#' @export
#'
#' @seealso `gen_varmod`
#' @examples
#' get_varmod(get_examples_path( file_type = "txt" ))
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated to homogenised writing process
#'
#'
#' @examples
#'  \dontrun{
#' get_var_mod(workspace,usms_file)
#' # ->
#' get_varmod(workspace,usms_file)
#' }
#'
#' @keywords internal
#'
#' @export
get_var_mod <- function(...) {

  lifecycle::deprecate_warn(
    "0.5.0",
    "get_var_mod()",
    "get_varmod()")
  get_varmod(...)
}
#' @export
#'
get_varmod= function(workspace,usms_file="var.mod", file_name = lifecycle::deprecated()){

  if (lifecycle::is_present(file_name)) {
    lifecycle::deprecate_warn("0.5.0", "get_varmod(file_name)", "get_varmod(usms_file)")
  } else {
    file_name <- usms_file # to remove when we update inside the function
  }


  file_path <- file.path(workspace, file_name)

  if(!file.exists(file_path)){
    stop(paste(file_path,": does not exist !"))
  }

  readLines(file_path)
}
