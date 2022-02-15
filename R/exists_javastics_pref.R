#' @title Evaluate if model preferences have been set
#'
#' @description Testing if preferences.xml file exist in JavaSTICS installation folder
#'
#' @param javastics_path JavaStics installation folder
#'
#' @examples
#' \dontrun{
#' exists_pref <- exists_javastics_pref("path/to/JavaSTICS-v131-stics-v841")
#'}
#'
#' @return logical value, TRUE if file exists, FALSE otherwise
#'
#' @keywords internal
exists_javastics_pref <- function(javastics_path){
  # checking javastics path
  check_java_path(javastics_path)

  # Returning if file exists
  return(file.exists(file.path(javastics_path,"config","preferences.xml")))
}
