#' Get the compatible Stics versions list
#'
#' @description Get the versions of stics that are fully compatible
#' with this package.
#'
#' @return A named list with the STICS versions compatible with this package
#' ($versions_list), and the latest version in use ($latest_version).
#'
#' @examples
#' \dontrun{
#' SticsRFiles::get_stics_versions_compat()
#'
#' #> $versions_list
#' #> [1] "V8.5" "V9.0" "V9.1"
#' #>
#' #> $latest_version
#' #> [1] "V9.1"
#'}
#'
#' @export
#'
#'
# TODO: may be in stics_versions_utils get_stics_versions_compat
# & new function check version
get_stics_versions_compat <- function() {

  # Getting versions list
  ver_info <- get_versions_info()
  versions_names <- ver_info$versions
  num_versions <- as.numeric(gsub(pattern = "^[V]","",versions_names))

  # Getting the latest version string
  latest_version <- versions_names[ num_versions == max(num_versions) ]

  # List of versions strings ans latest version string
  versions <- list(versions_list = versions_names, latest_version = latest_version )

  return(versions)
}


#' Checking the validity of a given version code
#'
#' @param version An optional version name as listed in
#' get_stics_versions_compat() return
#'
#' @return A valid version string
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' SticsRFiles:::check_version_compat()
#'
#' [1] "V9.1"
#'
#' check_version_compat(version = "V8.5")
#' [1] "V8.5"
#'
#' }
check_version_compat <- function(version_name = "latest") {

  versions <- get_stics_versions_compat()
  if (version_name == "latest") return(versions$latest_version)

  if ( version_name %in% versions$versions_list) return(version_name)

  stop(version_name,": is an unknown version!")
}


#' Getting versions data (versions strings and examples files directories list)
#'
#' @param version_name Optional version string (i.e. "VX.Y")
#'
#' @param versions_dir Optional, either an `extdata` directory path
#' of the installed SticsRFiles library (default) or of the package project
#'
#' @return A data.frame with versions data
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' SticsRFiles:::get_versions_info()
#'
#' versions compat  csv  obs  sti  txt           xml       xml_tmpl   xl
#' 1     V8.5   V8.5 V8.5 BASE BASE V8.5 examples/V8.5 templates/V8.5 BASE
#' 2     V9.0   V9.0 V9.0 BASE BASE V9.0 examples/V9.0 templates/V9.0 BASE
#' 3     V9.1   V9.1 V9.1 BASE BASE V9.1 examples/V9.1 templates/V9.1 BASE
#'
#' get_versions_info( version_name = "V8.5")
#'
#' versions compat  csv  obs  sti  txt           xml       xml_tmpl   xl
#' 1     V8.5   V8.5 V8.5 BASE BASE V8.5 examples/V8.5 templates/V8.5 BASE
#'
#'
#' get_versions_info(version_name = "V8.5",
#'                   versions_dir = "path/to/SticsRFilesproject/inst/extdata")
#'
#'
#'}
get_versions_info <- function(version_name = NULL, location = "install") {

  # Getting available versions info from a file
  ver_file <- get_versions_file_path(location = location)

  if (!file.exists(ver_file)) return()

  ver_info <- utils::read.csv2(file = ver_file, stringsAsFactors = FALSE,
                               colClasses = "character")

  # Set NA where no information is given
  ver_info[ver_info[]==""]=NA

  # Returning the full data.frame for all versions
  if (base::is.null(version_name)) return(ver_info)

  # Selecting data according to the desired version
  if (version_name %in% ver_info$versions) {
    return(ver_info[ver_info$versions == version_name,])
  }

  # Nothing to return for unknown version
  return()
}




#' Getting the csv file name storing versions information
#'
#' @return file name
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_versions_file_name()
#' }
get_versions_file_name <- function() {

  return("stics_versions_info.csv")

}

