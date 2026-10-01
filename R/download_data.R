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
  raise_error = FALSE
) {
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
  error_msg <- paste(
    "Error: no available data for ",
    example_dirs
  )
  if (base::is.null(dirs_str)) {
    if (raise_error) {
      stop(error_msg, call. = FALSE)
    } else {
      #message(error_msg)
      return(invisible())
    }
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

  data_url <- get_data_url(branch)

  # if the branch doesn't exist
  # testing internet availability:
  error_msg <- paste(
    "The internet resource could not be reached.",
    "Check internet connection, or resource url."
  )
  if (is.null(data_url)) {
    if (raise_error) {
      stop(error_msg, call. = FALSE)
    } else {
      message(error_msg)
      return(invisible())
    }
  }

  #
  file_name <- basename(data_url)

  # directory where to unzip the archive
  data_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  # Local archive file path
  data_zip_path <- normalizePath(
    file.path(data_dir, file_name),
    winslash = "/",
    mustWork = FALSE
  )

  # Download query for getting the master.zip
  try_ret <- try(
    suppressWarnings(utils::download.file(
      url,
      data_zip_path
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

  # Listing the archive content
  df_name <- utils::unzip(data_zip_path, exdir = data_dir, list = TRUE)

  # Creating files list to extract from dirs strings
  arch_files <- unlist(lapply(
    dirs_str,
    function(x) grep(pattern = x, x = df_name$Name, value = TRUE)
  ))

  # No data corresponding to example_dirs request in the archive !
  if (!length(arch_files)) {
    message(
      "No available data for example(s) in the downloaded archive, version: ",
      example_dirs,
      ",",
      stics_version
    )
    return(invisible())
  }

  # Checking if the download was successful
  # If not, returning an error message or raising an error
  error_msg <- paste(
    "No available data for example(s) in the downloaded archive, version: ",
    example_dirs,
    ",",
    stics_version
  )

  if (!length(arch_files)) {
    if (raise_error) {
      stop(error_msg, call. = FALSE)
    } else {
      message(error_msg)
      return(invisible())
    }
  }

  # Finally extracting data and removing the archive
  utils::unzip(data_zip_path, exdir = data_dir, files = arch_files)
  unlink(data_zip_path)

  # Returning the path of the folder where data have been extracted
  normalizePath(file.path(data_dir, arch_files[1]), winslash = "/")
}


#' Getting valid directories string for download from SticsRPacks `data`
#' repository
#'
#' @param dirs Directories names of the referenced use cases (optional),
#' starting with "study_case_"
#' @param stics_version An optional version string
#' within those given by get_stics_versions_compat()$versions_list
#' @param verbose logical flag for activating messages display (TRUE) or not
#' (FALSE, default value)
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
get_referenced_dirs <- function(
  dirs = NULL,
  stics_version = NULL,
  verbose = FALSE
) {
  # Loading csv file with data information
  ver_data <- get_versions_info(stics_version = stics_version)
  if (base::is.null(ver_data)) {
    if (verbose)
      message("No examples data referenced for version: ", stics_version)
    return(invisible())
  }

  dirs_names <- grep(pattern = "^study_case", x = names(ver_data), value = TRUE)
  if (base::is.null(dirs)) {
    dirs <- dirs_names
  }
  dirs_idx <- dirs_names %in% dirs

  # Not any existing use case dir found
  if (!any(dirs_idx)) {
    if (verbose)
      message("Not any existing use case for version: ", stics_version)
    return(invisible())
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
  version_data <- ver_data %>%
    dplyr::select(dplyr::any_of(dirs))

  # Compiling referenced directories/version strings, for existing version
  is_na <- base::is.na(version_data)

  dirs_str <-
    sprintf("%s/%s", names(version_data)[!is_na], version_data[!is_na])

  dirs_str
}

get_data_url <- function(branch = "master") {
  url_str <- paste0(
    "https://github.com/SticsRPacks/data/archiv/",
    branch,
    ".zip"
  )
  # If the response status is not a success
  if (httr::GET(url_str)$status_code != 200) return(invisible())

  url_str
}

get_default_branch <- function() {
  # Getting the default branch name from the remote repository
  # this has been commented bc on windows it does not work
  # the shell command returns the right result but not the R command
  # using system !
  # system(
  #   "git ls-remote --symref https://github.com/SticsRPacks/data HEAD | awk -F'[/\t]' 'NR == 1 {print $3}'",
  #   intern = TRUE
  # )
  "master"
}
