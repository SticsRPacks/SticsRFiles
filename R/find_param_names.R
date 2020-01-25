#' Finding parameters names using partial search words
#'
#' @param name Name or partial name or a vector of
#' @param version Optional, Stics version.
#' Only the 2 last are referenced: V9.0, V9.1 (default value)
#'
#' @return A data.frame containing parameters names,
#' their origin (file name) and their bounds.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' find_param_names(name = "albedo")
#'
#' find_param_names(name = "albedo", version = "V9.0")
#'
#' find_param_names(name = c("albedo", "latitude", "humcapil"))
#'
#' find_param_names(name = c("al", "hum"))
#'
#' }
#'
#'
find_param_names <- function(name, version = NULL) {

  param_names <- find_param(name = name,
                                version = version,
                                kind = "parameter")
  return(param_names)
}
