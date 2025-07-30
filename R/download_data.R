#' Download example USMs
#'
#' @description Download locally the example data from the
#' [data repository](https://github.com/SticsRPacks/data) in the SticsRPacks
#' organization.
#'
#'
#' @param branch Git branch name (optional)
#' @param out_dir Path of the directory where to download the data
#' (optional, default: tempdir())
#' @param example_dirs List of use case directories names (optional)
#' @param stics_version Name of the STICS version (optional)
#' The default value is the latest version returned by
#' get_stics_versions_compat().
#' @param raise_error Logical, if TRUE, an error is raised instead
#' of message when FALSE (default)
#'
#' @return The path of the folder data have been downloaded into or NULL
#' if the download fails and raise_error is FALSE.
#'
#' @export
#'
#' @examples
#'
#' # Getting data for a given example : study_case_1 and a given STICS version
#' download_data(example_dirs = "study_case_1", stics_version = "V9.0")
#' # raising an error instead of a message
#' download_data(
#'   example_dirs = "study_case_1", stics_version = "V9.0",
#'   raise_error = TRUE
#' )
download_data <- function(
    branch = NULL,
    out_dir = tempdir(),
    example_dirs = NULL,
    stics_version = "latest",
    raise_error = FALSE) {
  # getting the default branch name if not specified
  if (is.null(branch)) {
    branch <- get_default_branch()
  }

  # Setting version value from input for version == "latest"
  if (is.null(stics_version) || stics_version == "latest") {
    stics_version <- get_stics_versions_compat()$latest_version
  }

  # Getting path string(s) from examples data file
  dirs_str <- get_referenced_dirs(
    dirs = example_dirs,
    stics_version = stics_version
  )

  # Not any examples_dirs not found in example data file
  if (base::is.null(dirs_str)) {
    stop("Error: no available data for ", example_dirs)
  }

  # Checking if the path exist(s), if a prior extraction has been done
  prev_data_dir <- file.path(
    out_dir,
    "data-master",
    dirs_str
  )

  # All directories already exist, exiting
  if (all(file.exists(prev_data_dir))) {
    return(prev_data_dir)
  }

  url <- get_data_url(branch)
  file_name <- basename(url)
  # directory where to unzip the archive
  data_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  # Archive file path
  data_dir_zip <- normalizePath(
    file.path(data_dir, file_name),
    winslash = "/",
    mustWork = FALSE
  )

  # Download query for getting the master.zip
  try_ret <- try(
    suppressWarnings(utils::download.file(
      url,
      data_dir_zip
    )),
    silent = TRUE
  )

  error_msg <- paste(
    "Error while downloading data from GitHub.",
    "Check internet connection, or resource availability."
  )

  # Checking if the download was successful
  # If not, returning an error message or raising an error
  if (inherits(try_ret, "try-error")) {
    if (raise_error) {
      stop(error_msg, call. = FALSE)
    } else {
      message(error_msg)
      return(invisible())
    }
  }

  # Unzipping the archive
  df_name <- utils::unzip(data_dir_zip, exdir = data_dir, list = TRUE)

  # Creating files list to extract from dirs strings
  arch_files <- unlist(lapply(
    dirs_str,
    function(x) grep(pattern = x, x = df_name$Name, value = TRUE)
  ))

  # No data corresponding to example_dirs request in the archive !
  if (!length(arch_files)) {
    warning(
      "No available data for example(s), version: ",
      example_dirs,
      ",",
      stics_version
    )
    return(invisible())
  }

  # Finally extracting data and removing the archive
  utils::unzip(data_dir_zip, exdir = data_dir, files = arch_files)
  unlink(data_dir_zip)

  # Returning the path of the folder where data have been downloaded
  normalizePath(file.path(data_dir, arch_files[1]), winslash = "/")
}


#' Getting valid directories string for download from SticsRPacks `data`
#' repository
#'
#' @param dirs Directories names of the referenced use cases (optional),
#' starting with "study_case_"
#' @param stics_version An optional version string
#' within those given by get_stics_versions_compat()$versions_list
#'
#' @return Vector of referenced directories string (as "study_case_1/V9.0")
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Getting all available directories from the data repository
#' get_referenced_dirs()
#'
#' # Getting directories for a use case
#' get_referenced_dirs("study_case_1")
#'
#' # Getting directories for a use case and a version
#' get_referenced_dirs("study_case_1", "V9.0")
#'
#' get_referenced_dirs(c("study_case_1", "study_case_2"), "V9.0")
#' }
#'
get_referenced_dirs <- function(dirs = NULL, stics_version = NULL) {
  # Loading csv file with data information
  ver_data <- get_versions_info(stics_version = stics_version)
  if (base::is.null(ver_data)) {
    stop("No examples data referenced for version: ", stics_version)
  }

  dirs_names <- grep(pattern = "^study_case", x = names(ver_data), value = TRUE)
  if (base::is.null(dirs)) dirs <- dirs_names
  dirs_idx <- dirs_names %in% dirs

  # Not any existing use case dir found
  if (!any(dirs_idx)) {
    return()
  }

  # Filtering existing directories in examples data
  if (!all(dirs_idx)) {
    dirs <- dirs_names[dirs_idx]
  }

  # Only dirs, returned if no specified version
  if (base::is.null(stics_version)) {
    return(dirs)
  }

  # Getting data according to version and directories
  version_data <- ver_data %>% dplyr::select(dplyr::any_of(dirs))

  # Compiling referenced directories/version strings, for existing version
  is_na <- base::is.na(version_data)

  dirs_str <-
    sprintf("%s/%s", names(version_data)[!is_na], version_data[!is_na])

  dirs_str
}

get_data_url <- function(branch = "master") {
  paste0("https://github.com/SticsRPacks/data/archive/", branch, ".zip")
}

get_default_branch <- function() {
  system(
    "git ls-remote --symref https://github.com/SticsRPacks/data HEAD | awk -F'[/\t]' 'NR == 1 {print $3}'",
    intern = TRUE
  )
}
