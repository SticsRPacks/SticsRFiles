#' Finding parameters names or formalism using partial search words
#'
#' @param name Name or partial name or a vector of
#' @param version Optional, Stics version.
#' Only the 2 last are referenced: V9.0, V9.1 (default value)
#' @param kind Kind of information to be retrieved for parameters
#' among "paramete", "formalism"
#'
#' @param fixed Logical, if TRUE, the exact name is searched
#'
#' @return A data.frame containing parameters names,
#' their origin (file name) and their bounds or a named list
#' of files containing formalisms and attached parameters names.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' find_param(name = "albedo")
#'
#' find_param(name = "albedo", kind = "formalism)
#'
#' find_param(name = "albedo", version = "V9.0")
#'
#' find_param(name = c("albedo", "latitude", "humcapil"))
#'
#' find_param(name = c("albedo", "latitude", "humcapil"),
#' kind = "formalism)
#'
#' }
#'
#'
find_param <- function(name,
                       version=NULL,
                       kind = "parameter",
                       fixed = FALSE) {

  kinds <- c("parameter", "formalism")

  # Checking kind
  if (! kind %in% kinds ) {
    stop(paste("Unknown kind of parameter(s) information to retrieve:",kind))
  }

  # Just in case
  name <- unique(name)

  # Check Stics version
  version <- get_xml_stics_version(version)

  # Getting XML examples files dir from the package
  xml_dir <- system.file(file.path("extdata","xml","examples",version),
                         package = "SticsRFiles")

  # Getting the XML files list
  files_list <- list.files(path = xml_dir,
                           pattern = "\\.xml$",
                           full.names = TRUE)

  # Not any files found !
  if (length(files_list) == 0) {
    stop("Examples XML files not found in the package !")
  }

  # Getting parameters names bounds and file
  param_names <- get_param_names_xml(xml_file = files_list,
                                     name = name,
                                     fixed = fixed)

  # Not any parameters found
  if (all(dim(param_names)==0)) {
    warning(paste("Not any parameter found for Stics version: ", version))
  }

  # Returning parameters information
  if (kind == "parameter") return(param_names)

  # Getting parameter formalism information
  files_list <- file.path(xml_dir, unique(param_names$file))
  param_formalism <- get_formalisms_xml(xml_file = files_list,
                                       par_name = param_names$name)

  return(param_formalism)
}
