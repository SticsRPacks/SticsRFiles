#' Get desired STICS outputs
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated to homogenised writing process
#'
#'
#' @examples
#' \dontrun{
#' get_var_mod(workspace, usms_file)
#' # ->
#' get_varmod(workspace, usms_file)
#' }
#'
#' @keywords internal
#'
#' @export
get_var_mod <- function(...) {
  lifecycle::deprecate_warn(
    "0.5.0",
    "get_var_mod()",
    "get_varmod()"
  )
  get_varmod(...)
}


#' Get desired STICS outputs
#'
#' @description Get the STICS output variables (from var.mod file)
#'
#' @param workspace Path of the directory containing the Stics var.mod file
#' @param file_name file name to read (without path, default value: "var.mod")
#'
#' @return The variables that will be returned by STICS
#' @export
#'
#' @seealso `gen_varmod`
#' @examples
#' get_varmod(get_examples_path(file_type = "txt"))
get_varmod <- function(workspace,
                       file_name = "var.mod") {
  file_path <- file.path(workspace, file_name)

  if (!file.exists(file_path)) {
    stop(paste(file_path, ": does not exist !"))
  }

  readLines(file_path)
}
