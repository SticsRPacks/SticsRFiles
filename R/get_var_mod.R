
#' Get desired STICS outputs
#'
#' @description Get the variables STICS will return ("var.mod" file)
#'
#' @param filepath The path to the "var.mod" file.
#'
#' @return The variables that will be returned by STICS
#' @export
#'
#' @seealso `gen_varmod`
#' @examples
#' get_var_mod(system.file("extdata/txt/V8.5", package = "SticsRFiles"))
#'
get_var_mod= function(workspace,file_name="var.mod"){
  file_path <- file.path(workspace, file_name)

  if(!file.exists(file_path)){
    stop(paste(file_path,": does not exist !"))
  }

  readLines(file_path)
}
