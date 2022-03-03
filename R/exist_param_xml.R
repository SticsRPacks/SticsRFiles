#' Checking existing parameters names in XML files
#'
#' @param parameter Parameter name or a vector of
#' @param version Optional, Stics version.
#' Only the 2 latest are referenced: V9.0, V9.1 (default value)
#'
#' @return A logical vector of existing parameters
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' exist_param_xml(parameter = "albedo")
#'
#' exist_param_xml(parameter = "albedo", version = "V9.0")
#'
#' exist_param_xml(parameter = c("albedo", "latitude", "humcapil"))
#'
#' exist_param_xml(parameter = c("albedo", "latitude", "humcapi"))
#' }
#'
exist_param_xml <- function(parameter,
                            version = NULL) {

  # Finding exact matchs in found names
  par_names <- get_param_data_df(
    parameter = parameter,
    version = version,
    exact = TRUE
  )$name

  # Checking if any correspondence for each element of name
  exist_status <- unlist(lapply(parameter, function(x) any(par_names %in% x)))

  # Exiting status vector
  return(exist_status)
}
