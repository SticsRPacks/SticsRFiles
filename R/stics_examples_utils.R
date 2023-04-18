#' Getting examples files path attached to a STICS version for a given file type
#'
#' @param file_type A file type string among files types
#' ("csv", "obs", "sti", "txt", "xml")
#' @param stics_version Name of the Stics version. Optional, by default
#' the latest version returned by `get_stics_versions_compat()` is used.
#' @param version_name `r lifecycle::badge("deprecated")` `version_name` is no
#'   longer supported, use `stics_version` instead.
#'
#' @return A directory path for examples files for given file type and STICS
#' version or NULL if missing file_type (types list is displayed)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_examples_path(file_type = "csv")
#'
#' get_examples_path(file_type = "csv", stics_version = "V8.5")
#' }
get_examples_path <- function(file_type, stics_version = "latest",
                              version_name = lifecycle::deprecated()) {
  if (lifecycle::is_present(version_name)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_examples_path(version_name)",
      "get_examples_path(stics_version)"
    )
  } else {
    # to remove when we update inside the function
    version_name <- stics_version
  }

  # Getting files types list
  example_types <- get_examples_types()

  # If not any arguments : displaying files types list
  if (missing(file_type)) {
    cat("Available files types: ", paste(get_examples_types(), collapse = ","))
    return(invisible())
  }

  # Checking if file_type exists
  if (!file_type %in% example_types) stop("Unknown file_type: ", file_type)

  # Validating the version string
  version_name <- check_version_compat(version_name)

  # Checking if files available for the given version
  ver_data <- get_versions_info(stics_version = version_name)
  if (base::is.null(ver_data))
    stop("No examples available for version: ", version_name)

  # Getting files dir path for the given type
  version_dir <- ver_data[[file_type]]
  file_str <- gsub(pattern = "(.*)_.*", x = file_type, replacement = "\\1")

  # unzipping data for file_str files type and version (version_dir)
  examples_path <- file.path(unzip_examples(file_str), version_dir)

  # Not existing type
  if (examples_path == "")
    stop("Not any available ",
         file_type,
         " examples for version: ",
         version_name)

  # Returning the examples files dir path for the given type
  return(examples_path)
}

# TODO: evaluate if usefull ?
list_examples_files <- function(file_type,
                                version_name = "latest",
                                full_names = TRUE) {
  examples_path <- get_examples_path(file_type = file_type,
                                     stics_version = version_name)


  files_list <- list.files(pattern = "\\.[a-zA-Z]+$",
                           path = examples_path,
                           full.names = full_names)

  return(files_list)
}


get_examples_types <- function() {
  file_types <- c("csv", "obs", "sti", "txt", "xml", "xl", "xml_tmpl", "xsl")
  return(file_types)
}


#' Unzip files archive if needed and return examples files path
#' in extdata directory
#'
#' @param examples_type_path library path for examples files set
#' @param version_dir version directory names of the example files
#'
#' @return library examples files path
#'
#' @keywords internal
#'
# @examples
unzip_examples <- function(files_type, version_dir) {

  ex_path <- system.file("extdata",
                         package = "SticsRFiles")

  dir_path <- file.path(ex_path, files_type)

  if (dir.exists(dir_path)) return(dir_path)

  zip_path <- file.path(ex_path, paste0(files_type, ".zip"))

  if (!file.exists(zip_path))
    stop("Missing file: ", zip_path)

  utils::unzip(zipfile = zip_path, exdir = ex_path)

  return(dir_path)
}
