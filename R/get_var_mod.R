
#' Get desired STICS outputs
#'
#' @description Get the variables STICS will return ("var.mod" file)
#'
#' @param workspace Stics or JavaStics workspace path
#' @param file_name file name to generate (default value: "var.mod")
#'
#' @return The variables that will be returned by STICS
#' @export
#'
#' @seealso `gen_varmod`
#' @examples
#' get_var_mod(get_examples_path( file_type = "txt" ))
#'
get_var_mod= function(workspace,file_name="var.mod"){
  file_path <- file.path(workspace, file_name)

  if(!file.exists(file_path)){
    stop(paste(file_path,": does not exist !"))
  }

  readLines(file_path)
}
