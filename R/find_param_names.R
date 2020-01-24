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
find_param_names <- function(name, version=NULL) {

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
  if (length(files_list) == 0) stop("examples XML files not found in the package !")

  # Getting parameters names bounds and file
  param_names <- get_param_names_xml(xml_file = files_list,
                                     name = name)

  # Not any parameters found
  if (all(dim(param_names)==0)) {
    warning(paste("Not any parameter found for Stics version: ", version))
  }

  return(param_names)
}
