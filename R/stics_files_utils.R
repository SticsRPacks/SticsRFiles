#' Getting examples files path attached to a STICS version for a given file type
#'
#' @param file_type A file type string among files types or a vector of
#' ("csv", "obs", "sti", "txt", "xml")
#' @param stics_version Name of the STICS version. Optional, by default
#' the latest version returned by `get_stics_versions_compat()` is used.
#' @param overwrite TRUE for overwriting directory; FALSE otherwise
#'
#' @return A directory path for examples files for given file type and STICS
#' version or a vector of (for unknown file types "" is returned as path)
#'
#' @export
#'
#' @examples
#' get_examples_path(file_type = "csv")
#'
#' get_examples_path(file_type = c("csv", "sti"))
#'
#' get_examples_path(file_type = "csv", stics_version = "V8.5")
#'
get_examples_path <- function(
  file_type,
  stics_version = "latest",
  overwrite = FALSE
) {
  # Getting files types list
  example_types <- get_examples_types()

  # If not any arguments : displaying files types list
  if (missing(file_type)) {
    message(
      "Available files types: ",
      paste(get_examples_types(), collapse = ",")
    )
    return(invisible())
  }

  # Checking if all types in file_type exist
  files_type_idx <- file_type %in% example_types
  if (!all(files_type_idx)) {
    stop("Unknown file_type: ", file_type[!files_type_idx])
  }

  # Validating the version string
  stics_version <- check_version(stics_version)

  # Checking if files available for the given version
  ver_data <- get_versions_info(stics_version = stics_version)
  if (base::is.null(ver_data)) {
    stop("No examples available for version: ", stics_version)
  }

  # Getting files dir path for the given type
  version_dirs <- unlist(dplyr::select(ver_data, dplyr::all_of(file_type)))
  is_na_dirs <- is.na(version_dirs)

  if (any(is_na_dirs)) {
    stop(
      "Not any data in examples for ",
      paste(file_type[is_na_dirs], collapse = ", "),
      " and version ",
      stics_version
    )
  }

  files_str <- unlist(
    lapply(
      file_type,
      function(x) gsub(pattern = "(.*)_.*", x = x, replacement = "\\1")
    )
  )

  # Getting and storing path for each kind of file
  examples_path <- vector(mode = "character", length = length(files_str))
  for (i in seq_along(files_str)) {
    base_path <- unzip_examples(files_str[i], overwrite = overwrite)
    if (base_path == "") {
      examples_path[i] <- ""
    } else {
      examples_path[i] <- normalizePath(
        file.path(base_path, version_dirs[i]),
        winslash = "/",
        mustWork = FALSE
      )
    }
  }

  # Treating not existing directories for file_type
  exist_ex_path <- !(examples_path == "")
  if (!all(exist_ex_path)) {
    warning(
      "Not any available ",
      paste(file_type[!exist_ex_path], collapse = ", "),
      " examples for version: ",
      stics_version
    )
  }

  # Returning the examples files dir path for the given type
  return(invisible(examples_path))
}

# TODO: evaluate if it is useful ?
list_examples_files <- function(
  file_type,
  stics_version = "latest",
  full_names = TRUE
) {
  examples_path <- get_examples_path(
    file_type = file_type,
    stics_version = stics_version
  )

  files_list <- list.files(
    pattern = "\\.[a-zA-Z]+$",
    path = examples_path,
    full.names = full_names
  )

  return(files_list)
}


get_examples_types <- function() {
  c(
    "csv",
    "obs",
    "sti",
    "txt",
    "xml",
    "xl",
    "xml_tmpl",
    "xml_param",
    "xsl"
  )
}


#' Unzip files archive if needed and return examples files path
#' in extdata directory
#'
#' @param files_types type of file of examples files set
#' @param version_dir version directory names of the example files
#' @param overwrite TRUE for overwriting directory; FALSE otherwise
#'
#' @return library examples files path
#'
#' @keywords internal
#'
#' @noRd
#'
# @examples
unzip_examples <- function(files_type, version_dir, overwrite = FALSE) {
  ex_path <- system.file("extdata", package = "SticsRFiles")

  dir_path <- normalizePath(
    file.path(tempdir(), files_type),
    winslash = "/",
    mustWork = FALSE
  )

  if (dir.exists(dir_path) && !overwrite) {
    return(dir_path)
  }

  if (overwrite) {
    unlink(x = dir_path, recursive = TRUE)
  }

  zip_path <- file.path(ex_path, paste0(files_type, ".zip"))

  if (file.exists(zip_path)) {
    utils::unzip(zipfile = zip_path, exdir = tempdir())
  } else {
    dir_path <- ""
  }

  dir_path
}


#' Copy mod, obs, lai, and weather data files
#' @param workspace JavaSTICS xml workspace path
#' @param out_dir   Output directory path
#' @param file_type file type to copy among "mod", "obs", "clim"
#' @param javastics JavsSTICS folder path (Optional)
#' @param verbose   logical, TRUE for displaying a copy message
#' FALSE otherwise (default)
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return invisible copy statuses
#'
#' @keywords internal
#' @noRd
#'
workspace_files_copy <- function(
  workspace,
  out_dir,
  file_type = NULL,
  javastics = NULL,
  overwrite = FALSE,
  verbose = FALSE
) {
  # creating the output folder if it does not exist
  if (!dir.exists(out_dir)) dir.create(out_dir)

  # files types vector and associated regex
  file_types <- c("mod", "obs", "lai", "meteo")
  file_patt <- c("*.mod", "*.obs", "*.lai", "\\.[0-9]{4}$")
  file_desc <- c(
    "output definition (*.mod)",
    "observation (*.obs)",
    "LAI dynamics (*.lai)",
    "weather data (*.YYYY)"
  )

  # if file_type is not given, all files type are processed
  if (is.null(file_type)) {
    file_type <- file_types
  }

  # recursive call for a vector
  if (length(file_type) > 1) {
    stat_list <- vector(mode = "list", length(file_type))
    for (i in seq_along(file_type)) {
      stat_list[[i]] <- workspace_files_copy(
        workspace = workspace,
        file_type = file_type[i],
        javastics = javastics,
        out_dir = out_dir,
        overwrite = overwrite,
        verbose = verbose
      )
    }
    return(invisible(stat_list))
  }

  # Just in case if the func is used outside of the workspace upgrade
  type_idx <- file_types %in% file_type

  if (!any(type_idx)) {
    warning("The given file type does not exist: ", file_type, " nothing done!")
    return()
  }

  # Getting the files path list to copy
  patt <- file_patt[type_idx]
  files_list <- list.files(
    path = workspace,
    full.names = TRUE,
    pattern = patt
  )

  if ("mod" %in% file_type) {
    if (!is.null(javastics)) {
      javastics_files <- list.files(
        path = file.path(
          javastics,
          "config"
        ),
        full.names = TRUE,
        pattern = patt
      )
    } else {
      javastics_files <- character(0)
    }

    diff_files <- setdiff(
      basename(javastics_files),
      basename(files_list)
    )

    # completion of files list with javastics ones
    if (length(diff_files) > 0) {
      javastics_files <-
        javastics_files[basename(javastics_files) %in% diff_files]

      files_list <- c(files_list, javastics_files)
    }
  }

  # Not any file neither in javastics nor in the workspace directories
  if (length(files_list) == 0) {
    warning(
      paste0("Not any '", file_desc[type_idx], "' file to copy!"),
      " Neither in ",
      javastics,
      " nor in ",
      workspace
    )
    return()
  }

  # opy and treat of the copy return
  dest_files <- file.path(out_dir, basename(files_list))
  stat <- file.copy(
    from = files_list,
    to = dest_files,
    overwrite = overwrite
  )

  if (verbose) {
    message(paste("Copying", file_desc[type_idx], "files.\n"))
    print(dest_files)
  }

  if (!all(stat)) {
    warning(
      "Error when copying file(s): ",
      paste(basename(files_list[!stat]), collapse = ", "),
      "\nin\n",
      out_dir,
      "\n",
      "Consider to set as input: overwrite = TRUE"
    )
  }
  invisible(stat)
}

#' Check if mandatory parameters have been set with a correct value
#' (i.e. non empty value as defined in the `is_empty_value` function)
#' @param par_names a vector of names
#' @param xml_file an xml file template
#'
#' @returns A logical vector
#' @keywords internal
#' @noRd
#'
check_mandatory_parameters <- function(par_names, xml_file) {
  # Checking if par_names are well set in the xml file
  # extracting data from file
  file_par_values <- get_param_xml(file = xml_file, param = par_names)
  file_par_names <- names(file_par_values[[1]])
  # Getting value type in the xml file
  xml_param_types <- get_xml_param_type(
    xml_file = xml_file,
    param = file_par_names
  )

  if (is.null(xml_param_types)) return(invisible())

  # Checking empty values for par_names condsidered as mandatory parameters
  empty_xml_param_values <- is_empty_value(
    file_par_values[[1]][file_par_names],
    xml_param_types
  )

  empty_xml_param_values
}


#' Detecting if a vector of values contain empty values or not
#' @description
#' According to predefined empty value (STICS dependent)
#' for 3 types "character", "numeric", "integer", the function
#' evaluates if elements of `value` are matching predefined
#' empty values or not
#'
#' @param value parameter value or a vector of
#' @param expected_type expected parameter type or a vector of
#'
#' @returns a logical vector of empty values or not elements in `value`
#' @keywords internal
#' @noRd
#'
is_empty_value <- function(value, expected_type, par_names = NULL) {
  expected_types <- c("character", "numeric", "integer")

  if (length(value) != length(expected_type))
    stop("Vectors dimension consistency error !")

  if (!all(expected_type %in% expected_types)) stop("Type error !")

  if (is.null(par_names)) par_names <- names(value)

  if (length(value) > 1) {
    return(mapply(
      function(x, y, z) {
        is_empty_value(value = x, expected_type = y, par_names = z)
      },
      value,
      expected_type,
      par_names
    ))
  }

  if (is.list(value)) value <- unlist(value, use.names = TRUE)

  if (class(value) != expected_type)
    stop(
      "Type consistentcy error value, type or missing numeric value\n",
      "for parameter: ",
      par_names
    )

  if (is.character(value))
    is_empty_value <- value %in% c("", "-999", "999", "0", as.character(NA))
  if (is.numeric(value))
    is_empty_value <- value %in% c(-999, 999, 0, as.numeric(NA))

  is_empty_value
}

#' Getting parameters types as defined in a STICS xml file
#'
#' @description
#' Basically each parameter type of a STICS xml file is set
#' in a `format` attribute with 3 possible values integer, real and character
#' The function is getting from the file the attributes corresponding
#' to the vector of parameter names given as input. One of the `format`
#' attribute value does not match a R type `real` which is replaced
#' with `numeric`, and for simplification `integer`replaced with a
#' `numeric` type
#'
#' @param xml_file a xml file path
#' @param param a vector of parameters names
#'
#' @returns a vector of parameter types macthing R types
#' @keywords internal
#' @noRd
get_xml_param_type <- function(xml_file, param) {
  if (length(param) > 1) {
    return(
      lapply(param, function(x) get_xml_param_type(xml_file, x))
    )
  }
  xpath <- paste0('//param[@nom="', param, '"]')
  xml_type <- as.vector(get_attrs_values(
    object = xmldocument(xml_file),
    path = xpath,
    attr_list = "format"
  ))

  # checking if the parameter exists
  if (is.null(xml_type)) return()

  # mutate xml parameter "real" type to R "numeric" type
  if (xml_type == "real" | xml_type == "integer") xml_type <- "numeric"
  xml_type
}
