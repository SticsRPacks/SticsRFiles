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

  xml_doc= xmldocument(file_path)

  # Check which type of file it is, and get the right parameter name accordingly:
  xml_type= XML::xmlName(XML::xmlRoot(xml_doc@content))

  if(xml_type=="usms"){
    soil_list <- get_param_xml(file_path,"nomsol")[[1]]
  }else if(xml_type=="sols"){
    soil_list <- get_param_xml(file_path,"sol")[[1]]
  }else{
    stop("The file must be either a usm (usms) or a soil (sols) file")
  }

  # Not any name
  if(is.null(soil_list)) return(soil_list)

  # Filter soil list with partial match
  soil_list <- unlist(grep_usms(soil_list,name))

  return(soil_list)
}
