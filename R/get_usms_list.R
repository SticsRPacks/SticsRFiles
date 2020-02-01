#' @title Getting usms names list for an usms.xml file or a list of
#' @description Extracting a usm names list from an usms.xml file
#' @param usms_path usms dir path or usm file path
#' @param name Usm name or partial name
#' @param xml_name usms xml file name (optional, default = "usms.xml")
#'
#' @return A list with usms names
#'
#' @examples
#' path = system.file("extdata/xml/examples/V9.1", package = "SticsRFiles")
#' usms_list <- get_usms_list(usms_path = path)[[1]]
#'
#' usms_list <- get_usms_list(usms_path = path, name = "usm1")[[1]]
#'
#' usms_list <- get_usms_list(usms_path = path, name = c("usm1", "usm2"))[[1]]
#'
#' @export
#'
get_usms_list <- function(usms_path, name = NULL, xml_name = "usms.xml"){

  # TODO: add select key: i.e. get all usms names
  # with the same soil, plant 1,...

  # Getting usms files paths
  usms_xml_path <- lapply(usms_path, function(x) get_paths(x, xml_name))

  # Get usms list
  usms_list <- get_param_xml(usms_xml_path,"usm")

  # Not any name
  if (base::is.null(name)) return(usms_list)

  # Filter usms list with partial match
  usms_list <- lapply(usms_list, function(x) grep_usms(x,name))

  return(usms_list)
}

grep_usms <- function(usms_list, name) {

  usms_idx <- unlist(lapply(name, function(x) grep(pattern = x, x = usms_list)))

  #usms_idx <- grep(pattern = name, x = usms_list)

  if (!length(usms_idx)) return(NULL)

  usms_list <- usms_list[usms_idx]

  return(usms_list)

}

get_paths <- function(usms_path, xml_name = "usms.xml") {

  if (isdir(usms_path)) {
    return(file.path(usms_path,xml_name))
  }

  return(usms_path)

}
