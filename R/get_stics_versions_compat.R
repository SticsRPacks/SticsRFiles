#' Get the compatible STICS versions
#'
#' @description Get the versions of STICS that are fully compatible
#' with this package.
#'
#' @param version_index Absolute positive index, or negative relative index from
#' latest version
#'
#' @return A named list with the STICS versions compatible with this package
#' ($versions_list), and the latest version in use ($latest_version) or
#' an existing version selected using version_index.
#'
#' @examples
#' # Getting the complete versions list
#' get_stics_versions_compat()
#'
#' # Getting the first version
#' get_stics_versions_compat(1)
#'
#' # Getting the previous version of the latest one
#' get_stics_versions_compat(-1)
#'
#' @export
#'
#'
get_stics_versions_compat <- function(version_index = NULL) {
  # List of versions strings and latest version string
  versions_list <- get_versions_list()
  latest_version <- get_latest_version()

  # returning a list of versions string
  if (is.null(version_index)) {
    return(list(versions_list = versions_list, latest_version = latest_version))
  }

  # getting relative backwards versions
  nb_versions <- length(versions_list)

  if (version_index < 0) {
    if (version_index >= -nb_versions + 1) {
      return(versions_list[nb_versions + version_index])
    } else {
      return(invisible())
    }
  }

  # or absolute rank number
  if (version_index > 0) {
    if (version_index <= nb_versions) {
      return(versions_list[version_index])
    } else {
      return(invisible())
    }
  }
}

get_versions_list <- function(numeric = FALSE) {
  # Getting versions list
  versions_names <- get_versions_info()[["versions"]]
  if (!numeric) {
    return(versions_names)
  }
  return(get_version_num(versions_names))
}

get_latest_version <- function(numeric = FALSE) {
  versions_list <- get_versions_list(numeric = TRUE)
  latest <- max_version_num(versions_list)
  if (numeric) {
    return(latest)
  }
  as.character(latest)
}


#' Checking the validity of a given version code
#'
#' @param stics_version An optional version name as listed in
#' get_stics_versions_list() return
#'
#' @return A valid version string
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' check_version()
#' }
check_version <- function(stics_version = "latest") {
  if (stics_version == "latest") {
    stics_version <- get_latest_version()
  }
  if (
    get_version_num(stics_version) %in%
      get_version_num(get_versions_list(), numeric = FALSE)
  ) {
    return(stics_version)
  }
  stop(stics_version, ": is an unknown version!")
}


#' Getting versions data (versions strings and examples files directories list)
#'
#' @param stics_version Optional version string (i.e. "VX.Y")
#'
#' @param versions_dir Optional, either an `extdata` directory path
#' of the installed `SticsRFiles` library (default) or of the package project
#'
#' @return A data.frame with versions data
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' get_versions_info()
#'
#' get_versions_info(stics_version = "V8.5")
#'
#'
#' get_versions_info(
#'   stics_version = "V8.5",
#'   location = "path/to/SticsRFilesproject/inst/extdata"
#' )
#' }
get_versions_info <- function(stics_version = NULL, location = "install") {
  # Getting available versions info from a file
  ver_file <- get_versions_file_path(location = location)

  if (!file.exists(ver_file)) {
    return()
  }

  ver_info <- utils::read.csv2(
    file = ver_file,
    stringsAsFactors = FALSE,
    colClasses = "character"
  )

  # Set NA where no information is given
  ver_info[ver_info[] == ""] <- NA

  # Returning the full data.frame for all versions
  if (base::is.null(stics_version)) {
    return(ver_info)
  }

  # checking the stics_version :
  version_parts <- strsplit(stics_version, split = ".", fixed = TRUE)[[1]]
  version_parts_number <- length(version_parts)
  if (version_parts_number == 2) {
    replic <- 1
  } else if (version_parts_number == 1) {
    replic <- 2
  } else {
    replic <- 0
  }
  if (replic > 0) {
    stics_version <- paste(c(version_parts, rep("0", replic)), collapse = ".")
  }
  # fixing the version number to X.Y.Z from X, X.Y or from Vx, Vx.y,
  # stics_version <- complete_version_num(stics_version)

  # getting the version number and the id of the chosen version
  # according to numerical version (so 10.0 is equivalent to 10)
  ver_id <- which(
    get_version_num(stics_version) == get_version_num(ver_info$versions)
  )

  # Selecting data according to the desired version
  if (length(ver_id) > 0 && ver_id > 0) {
    # If the version is found, return the corresponding row
    return(ver_info[ver_id, ])
  }

  # Nothing to return for unknown version
  return()
}

#' Getting version number from the version string
#'
#' @param stics_version A STICS character or numerical version
#' (may be simplified, i.e. 10.1 or "10.1" or full character version
#' "10.0.0" )
#' @param numeric logical, TRUE for numerical output format,
#' FALSE for character output format
#' @return version number (numeric or character)
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_version_num("V10.0")
#' }
get_version_num <- function(stics_version, numeric = TRUE) {
  if (length(stics_version) > 1) {
    versions_list <- unlist(lapply(stics_version, function(x) {
      get_version_num(x, numeric = numeric)
    }))
    return(versions_list)
  }

  if (is.numeric(stics_version) && numeric) {
    char_version <- as.character(stics_version)
  }

  if (!inherits(stics_version, "svlist")) {
    # fixing the version number to X.Y.Z from X, X.Y or from Vx, Vx.y,
    char_version <- complete_version_num(stics_version)

    char_version <- gsub(
      pattern = "([V | v]{1})([0-9\\.]*)",
      x = char_version,
      replacement = "\\2"
    )
    # creating an object of type svlist
    v <- semver::parse_version(char_version)
  } else {
    v <- stics_version
    char_version <- as.character(v)
  }

  # output is a version string
  if (!numeric) {
    return(char_version)
  }

  # returning an object of class svlist with a char attribute
  attr(v, "version") <- char_version
  v
}

max_version_num <- function(versions_num) {
  max_version <- versions_num[[1]]
  if (length(versions_num) < 2) {
    return(max_version)
  }
  for (i in seq_along(versions_num)) {
    if (versions_num[[i]] > max_version) max_version <- versions_num[[i]]
  }
  max_version
}

#' Getting version string from the version number
#'
#' @param stics_version numeric (i.e. X.Y)
#'
#' @return version string
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_version_string()
#' }
get_version_string <- function(stics_version) {
  pattern <- "^[V | v]"

  if (
    is.character(stics_version) &&
      grepl(pattern = pattern, x = stics_version)
  ) {
    return(toupper(stics_version))
  }

  paste0("V", as.character(stics_version))
}


#' Getting the csv file name storing versions information
#'
#' @return file name
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_versions_file_name()
#' }
get_versions_file_name <- function() {
  return("stics_versions_info.csv")
}


complete_version_num <- function(stics_version) {
  if (is.numeric(stics_version)) stics_version <- as.character(stics_version)

  version_parts <- strsplit(stics_version, split = ".", fixed = TRUE)[[1]]
  version_parts_number <- length(version_parts)
  if (version_parts_number == 2) {
    replic <- 1
  } else if (version_parts_number == 1) {
    replic <- 2
  } else {
    return(stics_version)
  }
  paste(c(version_parts, rep("0", replic)), collapse = ".")
}
