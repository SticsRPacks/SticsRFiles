#' Getting examples files path attached to a STICS version for a given file type
#'
#' @param file_type A file type string among files types ("csv", "obs", "sti", "txt", "xml")
#' @param version_name An optional version string (default: latest version returned by get_stics_versions_compat())
#'
#' @return A directory path for examples files for given file type and STICS version or
#' NULL if missing file_type (types list is displayed)
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' get_examples_path( file_type = "csv" )
#'
#' [1] "/path/to/user-R-library/SticsRFiles/extdata/csv/V9.1"
#'
#' get_examples_path( file_type = "csv", version_name = "V8.5")
#'
#' [1] "/path/to/user-R-library/SticsRFiles/extdata/csv/V8.5"
#'
#' }
get_examples_path <- function(file_type, version_name = "latest") {

  # Getting files types list
  example_types <- get_examples_types()

  # If not any arguments : displaying files types list
  if( missing(file_type)) {
    cat("Available files types: ", paste(get_examples_types(), collapse=","))
    return(invisible())
  }

  # Checking if file_type exists
  if (! file_type %in% example_types) stop("Unknown file_type: ", file_type)

  # Validating the version string
  version_name <- check_version_compat(version_name)

  # Checking if files available for the given version
  ver_data <- get_versions_info(version_name = version_name)
  if (base::is.null(ver_data)) stop("No examples available for version: ",version_name)

  # Getting files dir path for the given type
  version_dir <- ver_data[[file_type]]
  file_str <- gsub(pattern = "(.*)_.*",x = file_type, replacement = "\\1")
  examples_path <- system.file(file.path("extdata", file_str, version_dir), package = "SticsRFiles")

  # Not existing type
  if (examples_path == "" ) stop("Not any available ",file_type, " examples for version: ",version_name)

  # Returning the examples files dir path for the given type
  return(examples_path)
}

# TODO: evaluate if usefull ?
list_examples_files <- function(file_type, version_name = "latest", full_names = TRUE) {

  examples_path <- get_examples_path(file_type = file_type, version_name = version_name)


  files_list <- list.files(pattern = "\\.[a-zA-Z]+$", path = examples_path, full.names = full_names)

  return(files_list)

}


get_examples_types <- function() {
  file_types <- c("csv", "obs", "sti", "txt", "xml", "xl", "xml_tmpl")
  return(file_types)
}
