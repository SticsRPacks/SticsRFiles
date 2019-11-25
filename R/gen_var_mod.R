#' @title Generating a var.mod type file
#' @description Generating a daily variable list file from variables names
#' @param workspace Stics or JavaStics workspace path
#' @param var_names vector of variables names
#' @param file_name file name to generate (default value: var.mod)
#'
#' @examples
#' \dontrun{
#'  gen_var_mod(".", c("var1","var2"))
#'  gen_var_mod("/path/to/stics/workspace", c("var1","var2"))
#' }
#' @export
gen_var_mod <- function(workspace, var_names, file_name ="var.mod") {


  if (! dir.exists(workspace)) {
    stop(paste(workspace,"directory does not exist !"))
  }

  file_path <- file.path(workspace, file_name)

  if (file.exists(file_path)) {
    file.remove(file_path)
  }

  # TODO: checking if par_names exist ?


  # checking par consistency
  nb_var <- length(var_names)

  # Writing file content
  con <- file(file_path, method = "w+")
  write(paste0(sprintf("%s",var_names), collapse = "\n"), con)
  close(con)


}
