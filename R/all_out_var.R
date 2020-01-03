#' Output variables data
#'
#' All output variables from STICS, with their units and brief description
#'
#' @format A data frame with seven columns:
#' \describe{
#'   \item{variable}{Variable name}
#'   \item{details}{a brief definition of the variable}
#'   \item{unit}{unit of the variable}
#'   \item{file}{the file where the variable comes from}
#'   \item{type}{the data type}
#'   \item{V1}{ }
#'   \item{V2}{ }
#' }
#'
"out_data"


#' Return all possible STICS outputs for var.mod
#'
#' @description Helper function to print the list of all possible variables to set as output
#' from the STICS model.
#'
#' @seealso \code{\link{set_out_var_txt}}
#'
#' @examples
#' \dontrun{
#'
#' SticsRFiles:::all_vars= all_out_var()
#'}
#'
#'
#' @keywords internal
#'
all_out_var= function(){
  out_data= NULL
  utils::data("out_data", envir = environment())
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
#' \dontrun{
#'
#' SticsRFiles:::find_out_var("lai")
#'
#'}
#'
#'
#' @keywords internal
#'
find_out_var = function(var=NULL){
  all_vars= all_out_var()
  if(!base::is.null(var)) {
    all_vars[grep(var, all_vars$variable,ignore.case = TRUE),]
  }else{
    all_vars
  }

}
