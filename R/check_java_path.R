#' @title Checking JavaStics directory content
#'
#' @description Checking if directory exists and if it contains JavaStics jar files
#' @details Rising an exception for each checking step !
#' @param javastics JavaStics installation root folder
#'
#' @examples
#'\dontrun{
#'check_java_path("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
#'}
#'
#' @keywords internal

check_java_path <- function(javastics){



  if (!file.exists(javastics)) {
    stop("The JavasStics folder doesn't exist : ",javastics)
  }

  # checking if it's a JavaStics root directory
  if (!file.exists(file.path(javastics,"JavaStics.exe")) &&
      !file.exists(file.path(javastics,"JavaStics.jar"))) {
    stop("This directory is not a JavaStics one: ",javastics)
  }

}
