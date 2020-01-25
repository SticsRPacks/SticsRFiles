#' Finding parameters names using partial search words
#'
#' @param name Name or partial name or a vector of
#' @param version Optional, Stics version.
#' Only the 2 last are referenced: V9.0, V9.1 (default value)
#'
#' @return A named list of files containing formalisms
#' and attached parameters names
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' find_param_formalisms(name = "albedo")
#'
#' find_param_formalisms(name = "albedo", version = "V9.0")
#'
#' find_param_formalisms(name = c("albedo", "latitude", "humcapil"))
#'
#' find_param_formalisms(name = c("al", "hum"))
#'
#' }
#'
#'
find_param_formalisms <- function(name, version = NULL) {

  param_formalisms <- find_param(name = name,
                                version = version,
                                kind = "formalism")

  return(param_formalisms)
}
