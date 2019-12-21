#' @title Getting usms names list for an usms.xml file or a list of
#' @description Extracting a usm names list from an usms.xml file
#' @param usms_path usms dir path or usm file path
#' @param xml_name usms xml file name (optional, default = "usms.xml")
#' @return A list with usms names
#' @examples
#' \dontrun{
#' usms_list <- get_usms_list("path/to/USM")
#' }
#'
#' @export
get_usms_list <- function(usms_path,xml_name="usms.xml"){

  if (isdir(usms_path)) {
    usms_xml_path=file.path(usms_path,xml_name)
  } else {
    usms_xml_path = usms_path
  }


  # new syntax with get_files_param_values
  #usms_list <- get_param_xml(usms_xml_path,"usm")[[1]]$values[[1]]

  usms_list <- get_param_xml(usms_xml_path,"usm")

  if (length(usms_list) == 1) usms_list <- unlist(usms_list)

  return(usms_list)
}
