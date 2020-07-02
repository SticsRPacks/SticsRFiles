#' @title Get the soil names for an usms.xml file
#'
#' @description Extracts the soil names from a "usms.xml" file, or from a soil file
#'
#' @param file_path The file path, either an usm file (e.g. "usms.xml"), or a soil file (e.g. "sols.xml")
#' @param name A vector of soil name(s) or partial name(s), optional
#'
#' @details The file given as the `file_path` is either a "usms" file type to get all the soils used in a particular USM,
#' or a soil file type ("sols") to get all soil types available in a soil file.
#'
#' @return A vector of soil names
#'
#' @examples
#' path = get_examples_path( file_type = "xml" )
#'
#' # Read from a usms file (soils used in a USM):
#' soil_list <- get_soils_list(file_path = file.path(path, "usms.xml"))
#'
#' # Read from a soil file (all soil types available in a soil file)
#' soil_list <- get_soils_list(file_path = file.path(path, "sols.xml"))
#'
#' soil_list <- get_soils_list(file_path = file.path(path, "usms.xml"), name = c("solcanne", "sole"))
#'
#' @export
#'
get_soils_list <- function(file_path, name = NULL){

  xml_name <- NULL

  # Detecting file type
  if ( is_usms_xml(file_path)) xml_name <- "nomsol"

  if ( is_sols_xml(file_path)) xml_name <- "sol"

  if (base::is.null(xml_name)) {
    stop("The file must be either a usm (usms) or a soil (sols) file")
  }

  return(find_usms_soils_names(file_path = file_path, xml_name = xml_name, name = name))
}





