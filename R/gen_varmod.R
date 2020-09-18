#' @title Generating a var.mod type file
#' @description Generating a daily variable list file from variables names
#' @param workspace Stics or JavaStics workspace path
#' @param var_names vector of variables names (see details)
#' @param append    Boolean. Append to existing file ?
#' @param file_name file name to generate (default value: "var.mod")
#' @param version   The version of the STICS model used (used to check variable names)
#' @param force     Force the variable writing even if the variable is not a STICS variable.
#'
#' @details Variable names can be found using `get_var_info()`. They are
#' checked before writting. If any variable names does not exist,
#' the function will still write the variables that exist,
#' `force= TRUE` may be used to write also variables that does not exist.
#'
#' @return Nothing. Writes to a file.
#'
#' @examples
#' \dontrun{
#'  gen_varmod(".", c("lai(n)","hauteur"))
#'  gen_varmod("/path/to/stics/workspace", c("lai(n)","hauteur"))
#'  # Add a variable to the others:
#'  gen_varmod(".", "masec(n)", append = TRUE)
#'  # NB: var.mod will have "lai(n)","hauteur" and "masec(n)"
#' }
#'
#' @export
#'
gen_varmod <- function(workspace, var_names, append=FALSE, file_name ="var.mod", version= "last", force= FALSE){
  # Checking if workspace exists
  if(!dir.exists(workspace)){
    stop(paste(workspace,": directory does not exist !"))
  }
  file_path <- file.path(workspace, file_name)

  # Check if the variable exist:
  var_exist= is_stics_var(var_names,version)


  if(any(!var_exist)&&isFALSE(force)){
    var_names= var_names[var_exist]
  }

  if (!length(var_names)) warning("Not any variable name to add to the var.mod file!")

  if(isTRUE(force)){
    var_names[var_exist]= var_to_stics_name(var_names[var_exist])
  }else{
    var_names= var_to_stics_name(var_names)
  }

  # Add possibility to append a variable to var.mod.
  if(isTRUE(append)){
    vars= readLines(file_path)
    commonvars= var_names%in%vars
    if(any(commonvars)){
      cli::cli_alert_warning("Variable{?s} {.var {var_names[commonvars]}} already in {.code var.mod}. Not repeating it.")
    }
    var_names= var_names[!commonvars]
    if(length(var_names)==0){
      invisible()
    }
  }

  cat(var_names,file= file_path, sep="\n", append=append)
}
