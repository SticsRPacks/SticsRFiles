#' @title Get the soil names for an usms.xml or sols.xml file
#'
#' @description Extracts the soil names from a USMs file, or from
#' a soil file
#'
#' @param file Either the path of an usm file or of a soil file.
#' @param soil Vector of soil names (or partial names). Optional,
#' if not provided, the function returns the names of all the soils included
#' in the given file.
#'
#' @details The file given as the `file_path` is either a "usms" file type
#' to get all the soils used in a particular USM, or a soil file type ("sols")
#' to get all soil types available in a soil file.
#'
#' @return A vector of soil names
#'
#' @examples
#' path <- get_examples_path(file_type = "xml")
#'
#' # Read from a usms file (soils used in a USM):
#' soil_list <- get_soils_list(file = file.path(path, "usms.xml"))
#'
#' # Read from a soil file (all soil types available in a soil file)
#' soil_list <- get_soils_list(file = file.path(path, "sols.xml"))
#'
#' soil_list <- get_soils_list(
#'   file = file.path(path, "usms.xml"),
#'   soil = c("solcanne", "sole")
#' )
#' @export
#'
get_soils_list <- function(
  file,
  soil = NULL
) {
  xml_name <- NULL

  xml_doc <- xmldocument(file)

  # Detecting file type to determine the parameter to use for the soil name
  if (is_stics_usms(xml_doc)) xml_name <- "nomsol"

  if (is_stics_sols(xml_doc)) xml_name <- "sol"

  if (base::is.null(xml_name)) {
    stop("The file must be either a usm (usms) or a soil (sols) file")
  }

  return(
    find_usms_soils_names(
      xml_doc = xml_doc,
      xml_name = xml_name,
      name = soil
    )
  )
}
