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
#' }
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
  num_versions <- as.numeric(gsub(pattern = "^[V]", "", versions_names))

  # Getting the latest version string
  latest_version <- versions_names[num_versions == max(num_versions)]

  # List of versions strings ans latest version string
  versions <- list(versions_list = versions_names, latest_version = latest_version)

  return(versions)
}


#' Checking the validity of a given version code
#'
#' @param stics_version An optional version name as listed in
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
#' }
check_version_compat <- function(stics_version = "latest") {
  versions <- get_stics_versions_compat()
  if (stics_version == "latest") {
    return(versions$latest_version)
  }

  if (stics_version %in% versions$versions_list) {
    return(stics_version)
  }

  stop(stics_version, ": is an unknown version!")
}


#' Getting versions data (versions strings and examples files directories list)
#'
#' @param stics_version Optional version string (i.e. "VX.Y")
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
#' get_versions_info(stics_version = "V8.5")
#'
#'
#' get_versions_info(
#'   stics_version = "V8.5",
#'   versions_dir = "path/to/SticsRFilesproject/inst/extdata"
#' )
#' }
get_versions_info <- function(stics_version = NULL, location = "install") {

  # Getting available versions info from a file
  ver_file <- get_versions_file_path(location = location)

  if (!file.exists(ver_file)) {
    return()
  }

  ver_info <- utils::read.csv2(
    file = ver_file, stringsAsFactors = FALSE,
    colClasses = "character"
  )

  # Set NA where no information is given
  ver_info[ver_info[] == ""] <- NA

  # Returning the full data.frame for all versions
  if (base::is.null(stics_version)) {
    return(ver_info)
  }

  # Selecting data according to the desired version
  if (stics_version %in% ver_info$versions) {
    return(ver_info[ver_info$versions == stics_version, ])
  }

  # Nothing to return for unknown version
  return()
}

#' Getting version number from the version string
#'
#' @param stics_version string (i.e. "VX.Y", or "vX.Y")
#' @param numeric logical, TRUE for numerical output format,
#' FALSE for character output format
#' @return version number (numeric or character)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_version_num()
#' }
get_version_num <- function(stics_version, numeric = TRUE) {
  if (is.numeric(stics_version) && numeric) {
    return(stics_version)
  }

  char_version <- gsub(pattern = "^[V | v]", "", stics_version)

  if (!numeric) {
    return(char_version)
  }

  as.numeric(char_version)
}


#' Getting version string from the version number
#'
#' @param stics_version numeric (i.e. X.Y)
#'
#' @return version string
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::get_version_string()
#' }
get_version_string <- function(stics_version) {
  pattern <- "^[V | v]"

  if (is.character(stics_version) & grepl(pattern = pattern, x = stics_version)) {
    return(toupper(stics_version))
  }

  paste0("V", as.character(stics_version))
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
