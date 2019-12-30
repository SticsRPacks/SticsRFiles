#' @title Generating a var.mod type file
#' @description Generating a daily variable list file from variables names
#' @param workspace Stics or JavaStics workspace path
#' @param var_names vector of variables names
#' @param file_name file name to generate (default value: var.mod)
#'
#' @return A logical status TRUE if successfull generation, FALSE otherwise
#'
#' @examples
#' \dontrun{
#'  gen_varmod(".", c("var1","var2"))
#'  gen_varmod("/path/to/stics/workspace", c("var1","var2"))
#' }
#'
#' @export
#'
gen_varmod <- function(workspace, var_names, file_name ="var.mod") {


  # Checking if workspace exists
  if (! dir.exists(workspace)) {
    stop(paste(workspace,": directory does not exist !"))
  }

  # Removing file if it exists
  file_path <- file.path(workspace, file_name)

  if (file.exists(file_path)) {
    file.remove(file_path)
  }

  # TODO: checking if var_names exist (according to Stics version)?
  # checking variables names consistency
  #nb_var <- length(var_names)

  # Writing file content
  con <- file(file_path, method = "w+")
  w <- try(write(paste0(sprintf("%s",var_names), collapse = "\n"), con))


  close(con)

  # Checking if any error writing the file
  if (methods::is(w,"try-error")) {
    return(invisible(FALSE))
  }


  return(invisible(TRUE))

}
