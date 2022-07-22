#' Getting package or library data path
#'
#' @param location The destination where to remove information and data
#' "install" for removing things from the installed SticsRFiles library (default),
#' "package" for removing them from the package project (in RStudio)
#'
#' @return A directory path
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_data_dir()
#'
#' SticsRFiles:::get_data_dir(location = "package")
#' }
get_data_dir <- function(location = "install") {
  if (location == "install") dest_dir <- system.file("extdata", package = "SticsRFiles")

  if (location == "package") {
    proj_dir <- rstudioapi::getActiveProject()
    pkg <- gsub(pattern = "\\.Rproj$", x = list.files(pattern = "\\.Rproj$", proj_dir), replacement = "")
    if (base::is.null(proj_dir) || pkg != "SticsRFiles") stop("Load the project SticsRFiles before proceeding !")
    dest_dir <- file.path(proj_dir, "inst", "extdata")
  }

  return(dest_dir)
}



#' Getting versions information csv file path according to its location
#'
#' @param location The destination where to remove information and data
#' "install" for removing things from the installed SticsRFiles library (default),
#' "package" for removing them from the package project (in RStudio)
#'
#' @return A file path
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_versions_file_path()
#'
#' SticsRFiles:::get_versions_file_path(location = "package")
#' }
get_versions_file_path <- function(location = "install") {
  file.path(get_data_dir(location = location), "versions", get_versions_file_name())
}
