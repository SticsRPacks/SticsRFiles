#' Checking existing parameters names in XML files
#'
#' @param param Parameter name or a vector of
#' @param stics_version Optional, Stics version.
#' Only the 2 latest are referenced: V9.0, V9.1 (default value)
#'
#' @return A logical vector of existing parameters
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' exist_param_xml(param = "albedo")
#'
#' exist_param_xml(param = "albedo", stics_version = "V9.0")
#'
#' exist_param_xml(param = c("albedo", "latitude", "humcapil"))
#'
#' exist_param_xml(param = c("albedo", "latitude", "humcapi"))
#' }
#'
exist_param_xml <- function(param,
                            stics_version = NULL) {

  # Finding exact matchs in found names
  par_names <- get_param_data_df(
    param = param,
    stics_version = stics_version,
    exact = TRUE
  )$name

  # Checking if any correspondence for each element of name
  exist_status <- unlist(lapply(param, function(x) any(par_names %in% x)))

  # Exiting status vector
  return(exist_status)
}
