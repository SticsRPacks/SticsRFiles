#' @title Getting usms names list
#' @description Extracting a usm names list from an usms.xml file
#' @param usms_path usms dir path or usm file path
#' @param xml_name usms xml file name (optional, default = "usms.xml")
#' @return A list with usms names
#' @examples
#' usms_list <- get_usms_list("/home/plecharpent/Work/JavaSTICS-v131-stics-v841/example")
#' @export
#'
# ----------------------------------------------------------------------
#  MODIFICATIONS (last commit)
#  $Date: 2019-07-28 19:37:15 +0200 (dim. 28 juil. 2019) $
#  $Author: plecharpent $
#  $Revision: 1554 $
# ----------------------------------------------------------------------

get_usms_list <- function(usms_path,xml_name="usms.xml"){

  if (isdir(usms_path)) {
    usms_xml_path=file.path(usms_path,xml_name)
  } else {
    usms_xml_path = usms_path
  }
  #xml_usms=xmldocument(usms_xml_path)
  #usms_list=getAttrsValues(xml_usms,'//usm','nom')

  # new syntax with get_files_param_values
  usms_list <- get_files_params_values(usms_xml_path,"usm")[[1]]$values[[1]]

  return(usms_list)
}
