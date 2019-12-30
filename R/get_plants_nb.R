#' @title Getting plants number per usm, all or selected from a given list
#'
#' @description Extracting plant number from usms.xml file data
#'
#' @param usm_xml_path Path of usms.xml file
#' @param usms_list Usms selection list inside usms from usms.xml file (optional)
#'
#' @return A numeric vector of plants number per usm
#'
#' @examples
#' \dontrun{

#' xml_usms <- system.file(paste0("extdata/xml/examples/V9.0/usms.xml"),
#' package = "SticsRFiles")
#' get_plants_nb(xml_usms)
#' get_plants_nb(xml_usms,"wheat")
#' get_plants_nb(xml_usms,c("wheat","intercrop_pea_barley"))
#' }
#'
#' @export
#'
get_plants_nb <- function(usm_xml_path,usms_list=c()){

  # Loading xml file as xmlDocument object
  xml_usms=xmldocument(usm_xml_path)

  # Getting plants nb per usm
  plants_nb=getValues(xml_usms,'//nbplantes')

  # Filtering using usms_list if needed
  if ( length(usms_list)!=0 ){
    usms=getAttrs(xml_usms,'//usm')
    usms_ids=unlist(lapply(usms,function(x) is.element(x,usms_list)))
    plants_nb=plants_nb[usms_ids]
  }

  return(as.numeric(plants_nb))
}
