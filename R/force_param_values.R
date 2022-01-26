#' @title Generates files to force parameters values in Stics simulations
#' @description Generates a param.sti file and sets code optim in new_travail.usm to force parameters values in Stics simulations (this function is typically called before `SticsOnR::run_stics()`)
#' @param workspace Path of the workspace containing the Stics (txt) input files.
#' @param values named vector of parameter values to force. See Details for more information.
#' @param param_values old_param `r lifecycle::badge("deprecated")` `old_param` is no
#'   longer supported, use new_param instead.
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
                               values,
                               param_values = lifecycle::deprecated()) {

  if (lifecycle::is_present(param_values)) {
    lifecycle::deprecate_warn("0.5.0", "force_param_values(param_values)", "force_param_values(values)")
  } else {
    param_values <- values # to remove when we update inside the function
  }

  if (is.null(param_values) || all(is.na(param_values))) {
    # remove param.sti in case of previous run using it ...
    if (suppressWarnings(file.remove(file.path(workspace,
                                               "param.sti")))) {
      tryCatch(set_codeoptim(workspace, value=0), error=function(cond) return(FALSE))
    }

  } else {

    # convert into vector in case a tibble is given instead of a vector
    param_values <- setNames(as.numeric(param_values),names(param_values))
    ind_non_na<-!is.na(param_values)
    param_values <- param_values[ind_non_na]

    # converting var names to Stics names
    stics_names <- col_names_to_var(names(param_values))

    ret <- gen_paramsti(workspace, stics_names, param_values)
    if ( ! ret ) {
     return(invisible(FALSE))
    }

    tryCatch(set_codeoptim(workspace, value=1), error=function(cond) return(FALSE))

  }

  return(invisible(TRUE))
}
