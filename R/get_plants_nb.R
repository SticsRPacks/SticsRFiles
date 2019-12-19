#' @title Getting plants number per usm, all or selected from a given list
#'
#' @description Extracting plant number from usms.xml file data
#'
#' @param usm_xml_path Path of usms.xml file
#' @param usms_list Usms selection list inside usms from usms.xml file (optional)
#'
#' @examples
#' \dontrun{
#' get_plants_nb("path/to/USM")
#' get_plants_nb("path/to/USM","ble")
#' get_plants_nb("path/to/USM",c("ble","mais"))
#' }
#'
#' @keywords internal
#'
get_plants_nb <- function(usm_xml_path,usms_list=c()){

  xml_usms=xmldocument(usm_xml_path)
  plants_nb=getValues(xml_usms,'//nbplantes')

  if (length(usms_list)!=0){
    usms=getAttrs(xml_usms,'//usm')
    usms_ids=unlist(lapply(usms,function(x) is.element(x,usms_list)))
    plants_nb=plants_nb[usms_ids]
  }
  return(as.numeric(plants_nb))
}
