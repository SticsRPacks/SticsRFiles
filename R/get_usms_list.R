#' @title Getting usms names list for an usms.xml file or a list of
#' @description Extracting a usm names list from an usms.xml file
#' @param usms_path usms dir path or usm file path
#' @param xml_name usms xml file name (optional, default = "usms.xml")
#'
#' @return A list with usms names
#'
#' @examples
#' path = system.file("extdata/xml/examples/V9.0", package = "SticsRFiles")
#' usms_list <- get_usms_list(path)[[1]]
#'
#'
#' @export
#'
get_usms_list <- function(usms_path,xml_name="usms.xml"){

  # TODO: add select key: i.e. get all usms names
  # with the same soil, plant 1,...
  if (isdir(usms_path)) {
    usms_xml_path=file.path(usms_path,xml_name)
  } else {
    usms_xml_path = usms_path
  }

  # Get usms list
  usms_list <- get_param_xml(usms_xml_path,"usm")

  return(usms_list)
}
