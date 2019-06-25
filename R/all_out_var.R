#' Return all possible STICS outputs for var.mod
#'
#' @description Helper function to print the list of all possible variables to set as output
#' from the STICS model.
#'
#' @seealso \code{\link{set_out_var}}
#'
#' @examples
#' library(sticRs)
#' All_vars= all_out_var()
#'
#' @export
#'
all_out_var= function(){
  data("out_data", envir = environment())
  out_data
}


#' Find output variable names for STICS
#'
#' @description Helper function that return the name used as input
#' for the STICS model with a partial match.
#'
#' @param Var Character vector with a (partial) STICS output variable name
#'
#' @details The function understand \code{\link[base]{regex}} as input.
#'
#' @seealso \code{\link{all_out_var}}
#'
#' @examples
#' library(sticRs)
#' find_STICS_var("lai")
#'
#' @export
#'
find_STICS_var= function(Var=NULL){
  All_vars= all_out_var()
  if(!is.null(Var)){
    All_vars[grep(Var, All_vars$variable,ignore.case = TRUE),]
  }else{
    All_vars
  }

}
