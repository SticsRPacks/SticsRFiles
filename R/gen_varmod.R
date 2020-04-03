#' @title Generating a var.mod type file
#' @description Generating a daily variable list file from variables names
#' @param workspace Stics or JavaStics workspace path
#' @param var_names vector of variables names (see details)
#' @param append    Boolean. Append to existing file ?
#' @param file_name file name to generate (default value: "var.mod")
#' @param version The version of the STICS model used (used to control the variable names)
#'
#' @details The variable names can be found using `find_var_info()`. The variable names are
#' checked before writting. If any variable names does not exist, the function will return
#' an error.
#'
#' @return Nothing. Writes to a file.
#'
#' @examples
#' \dontrun{
#'  gen_varmod(".", c("lai(n)","hauteur"))
#'  gen_varmod("/path/to/stics/workspace", c("lai(n)","hauteur"))
#'  # Add a variable to the others:
#'  gen_varmod(., "masec(n)", add= TRUE)
#'  # NB: var.mod will have "lai(n)","hauteur" and "masec(n)"
#' }
#'
#' @export
#'
gen_varmod <- function(workspace, var_names, append=FALSE, file_name ="var.mod", version= "last"){
  # Checking if workspace exists
  if(!dir.exists(workspace)){
    stop(paste(workspace,": directory does not exist !"))
  }
  file_path <- file.path(workspace, file_name)

  # Check if the variable exist:
  var_exist= is_stics_var(var_names,version)
  if(any(!var_exist)){
    return()
  }

  var_names= var_to_stics_name(var_names)

  # Add possibility to append a variable to var.mod.
  if(isTRUE(append)){
    vars= readLines(file_path)
    commonvars= var_names%in%vars
    if(any(commonvars)){
      cli::cli_alert_warning("Variable{?s} {.var {var_names[commonvars]}} already in {.code var.mod}. Not repeating it.")
    }
    var_names= var_names[!commonvars]
    if(length(var_names)==0){
      return()
    }
  }

  cat(var_names,file= file_path, sep="\n", append=append)
}
