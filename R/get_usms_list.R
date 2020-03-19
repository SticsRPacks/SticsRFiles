#' @title Getting usms names list for an usms.xml file
#'
#' @description Extracting a usm names list from an usms.xml file
#' @param usm_path usm file path (e.g. "usms.xml")
#' @param name A vector of usm name(s) or partial name(s), optional
#'
#' @return A vector of usm names
#'
#' @examples
#' path = system.file("extdata/xml/examples/V9.1", package = "SticsRFiles")
#'
#' usms_list <- get_usms_list(usm_path = file.path(path, "usms.xml"))
#'
#' usms_list <- get_usms_list(usm_path = file.path(path, "usms.xml"), name = c("usm1", "usm2"))
#'
#' @export
#'
get_usms_list <- function(usm_path, name = NULL){

  # TODO: add select key: i.e. get all usms names
  # with the same soil, plant 1,...

  # Get usms list
  usms_list <- get_param_xml(usm_path,"usm")[[1]]

  # Not any name
  if (base::is.null(name)) return(usms_list)

  # Filter usms list with partial match
  usms_list <- unlist(grep_usms(usms_list,name))

  return(usms_list)
}

grep_usms <- function(usms_list, name) {

  usms_idx <- unlist(lapply(name, function(x) grep(pattern = x, x = usms_list)))

  #usms_idx <- grep(pattern = name, x = usms_list)

  if (!length(usms_idx)) return(NULL)

  usms_list <- usms_list[usms_idx]

  return(usms_list)

}
