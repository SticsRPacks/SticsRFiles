#' @title Getting plants number per usm, all or selected from a given list
#'
#' @description Extracting plant number from usms.xml file data
#'
#' @param usm_xml_path Path of usms.xml file
#' @param usms_list Usms selection list inside usms from usms.xml file (optional)
#'
#' @examples
#' get_plants_nb("/home/plecharpent/Work/JavaSTICS-v131-stics-v841/example")
#' get_plants_nb("/home/plecharpent/Work/JavaSTICS-v131-stics-v841/example","ble")
#' get_plants_nb("/home/plecharpent/Work/JavaSTICS-v131-stics-v841/example",c("ble","mais"))
#'
#' @export
#'
# ----------------------------------------------------------------------
#  MODIFICATIONS (last commit)
#  $Date: 2019-07-28 19:37:15 +0200 (dim. 28 juil. 2019) $
#  $Author: plecharpent $
#  $Revision: 1554 $
# ----------------------------------------------------------------------
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
