#' @title Generates files to force parameters values in Stics simulations
#' @description Generates a param.sti file and sets code optim in new_travail.usm to force parameters values in Stics simulations (this function is typically called before `SticsOnR::run_stics()`)
#' @param workspace Stics workspace path
#' @param param_values named vector of parameter values to force. See Details for more information.
#'
#' @details This function operates on Stics text input files. Do not use it before calling `gen_usms_xml2txt()`, otherwise param.sti and new_travail.usm files will be overwritten.
#'
#'   This function has been created to be called before `SticsOnR::run_stics()`. It can not work with `SticsOnR::run_javastics()`, that will overwrite param.sti and new_travail.usm files.
#'
#'   param_values can contain NA. In this case, the corresponding parameter(s) will not be forced (default value(s), i.e. read in Stics input files, will be used).
#'   If param_values==NA or param_values==NULL, not any parameter will be forced (all default values used).
#'
#' @return A logical status TRUE if successfull, FALSE otherwise
#'
#' @examples
#' example_txt_dir <- get_examples_path(file_type="txt")
#' force_param_values(example_txt_dir, setNames(object=c(220,330), c("stlevamf", "stamflax")))
#'
#' @seealso `SticsOnR::run_stics()`
#'
#' @export
#'
force_param_values <- function(workspace,
                               param_values) {

  if (is.null(param_values) || all(is.na(param_values))) {
    # remove param.sti in case of previous run using it ...
    if (suppressWarnings(file.remove(file.path(workspace,
                                               "param.sti")))) {
      tryCatch(set_codeoptim(workspace, value=0), error=function(cond) return(FALSE))
      return(TRUE)
    }

  } else {

    ind_non_na<-!is.na(param_values)
    param_values <- param_values[ind_non_na]

    ret <- gen_paramsti(workspace, names(param_values), param_values)
    if ( ! ret ) {
     return(ret)
    }

    tryCatch(set_codeoptim(workspace, value=1), error=function(cond) return(FALSE))
    return(TRUE)

  }


}
