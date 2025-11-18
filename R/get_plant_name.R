#' Get plant names
#'
#' @description Get the plant name (and file name) for each usm in a workspace
#'
#' @param workspace      Path of a JavaSTICS workspace, or a vector of
#' (recursive call).
#' @param usms_filepath  Path of the usms file (`usms.xml`)
#' @param usm_name      Vector of usms to read (optional, used to filter usms)
#' @param javastics_path JavaSTICS installation path (Optional, needed if the
#' plant files are not in the `workspace` but rather in the JavaSTICS
#' default workspace). Only used to get the plants names.
#' @param verbose Logical value (optional), TRUE to display information
#' on error, FALSE otherwise (default)
#'
#'
#' @return A list of plant names for each usm with one value for sole crop
#' and two values for intercrop (principal, associated).
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' path <- get_examples_path(file_type = "xml")
#'
#' # Get plant names for all usms (no javastics path, so only the file
#' # name is returned):
#' get_plant_name(path)
#' # Get plant names only for banana:
#' get_plant_name(path, "banana")
#'
#' # Get plant names for banana with a javastics path, so the plant
#' # name is returned:
#' get_plant_name(path)
#' # NB: if the plant folder is in the workspace, no need to provide
#' # javastics_path
#' }
#'
get_plant_name <- function(
  workspace,
  usms_filepath,
  usm_name = NULL,
  javastics_path = NULL,
  verbose = TRUE
) {
  usms_path <- normalizePath(usms_filepath, mustWork = FALSE)

  if (!file.exists(usms_path)) {
    stop(usms_filepath, ": not found !")
  }

  # Getting usms names from the usms.xml file
  usms <- get_usms_list(file = usms_path)

  # Filtering USMs, if required
  if (!is.null(usm_name)) {
    usm_exist <- usm_name %in% usms

    # Some provided usms are not available:
    if (!all(usm_exist)) {
      if (verbose) {
        cli::cli_alert_danger(paste0(
          "The usm{?s} ",
          "{.val {usm_name[!usm_exist]}}",
          " d{?oes/o} not exist in the workspace!"
        ))
        cli::cli_alert_info("Usm{?s} found in the workspace: {.val {usms}}")
      }
      stop(usm_name, ": do(es) not match usms")
    }
    usms <- usm_name
  }

  # STEP 1: Get plant files per usm list
  plant_xml <- try(get_plant_files(
    usms_file = usms_path,
    usms = usms
  ))

  # Getting plant id instead if getting files names failed
  if (inherits(plant_xml, "try-error")) {
    plant_xml <- get_plant_id(plant_xml)
    names(plant_xml) <- usms
    if (verbose) {
      cli::cli_alert_warning("Error reading usms file, using dummy plant names")
    }
    return(plant_xml)
  }

  alert_msg <- paste0(
    "plant folder not found in the workspace, please add ",
    "{.code javastics_path} to use real plant names",
    " from javaStics."
  )

  if (is.null(javastics_path)) {
    plt_dir <- unique(file.path(workspace, "plant"))
    if (!all(dir.exists(plt_dir))) {
      cli::cli_alert_warning(alert_msg)
      return(plant_xml)
    }
  } else {
    plt_dir <- try(normalizePath(file.path(javastics_path, "plant")))
  }
  # Sorting list as workspace dirs
  if (length(workspace) > 1) {
    plant_xml <- plant_xml[basename(unique(workspace))]
  }

  # If there are several plt dirs, one per usm directory
  # sorting dirs as in plant_xml list
  if (length(plt_dir) > 1) {
    plt_dir <- plt_dir[unlist(lapply(
      names(plant_xml),
      function(x) grep(pattern = x, plt_dir)
    ))]
  } else {
    # otherwise plt_dir is replicated
    plt_dir <- rep(plt_dir, length(plant_xml))
  }

  # Getting plant codes
  plants_code <- vector("list", length(plant_xml))
  for (i in seq_along(plant_xml)) {
    plants_code[[i]] <- unlist(get_param_xml(
      file = file.path(plt_dir[i], plant_xml[[i]]),
      param = "codeplante"
    ))
  }

  # Keeping plant files names if an error occured
  # extracting plants code
  if (inherits(plants_code, "try-error")) {
    plants_code <- plant_xml
    if (verbose) {
      cli::cli_alert_warning(paste0(
        "Error reading plant names, ",
        "using plant file names for the",
        " output instead"
      ))
    }
  }
  names(plants_code) <- names(plant_xml)
  plants_code
}


#' Getting plant tag(s) from a plant files list by usm
#'
#' @param usms_plant a list of plants files names by usm
#'
#' @returns a list of plant tags by usm
#' @keywords internal
#'
# @examples
get_plant_id <- function(usms_plant) {
  lapply(usms_plant, function(x) {
    if (length(x) > 1) {
      c("plant_1", "plant_2")
    } else {
      c("plant_1")
    }
  })
}

#' Getting xml plant files by usm from an usms.xml file
#'
#' @param usms_file usms.xml file path
#' @param usms usms names to get, if NULL all teh usms are taken into account
#'
#' @returns A named list with usm names containing plant files names
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_plant_files("path/to/usms.xml")
#'
#' get_plant_files("path/to/usms.xml", c("usm1", "usm2")
#' }
#'
get_plant_files <- function(usms_file, usms = NULL) {
  all_usms <- get_usms_list(usms_file)

  if (is.null(usms)) {
    usms <- all_usms
  } else {
    ids <- usms %in% all_usms
    if (!all(ids)) warning("At least one usm name does not exist")
    usms <- usms[ids]
  }
  files_list <- lapply(
    usms,
    function(x) {
      plt_files <- get_param_xml(
        file = usms_file,
        param = "fplt",
        select = "usm",
        select_value = x
      )
      remove_null_value(plt_files$usms.xml$fplt)
    }
  )
  names(files_list) <- usms

  files_list
}

remove_null_value <- function(values) {
  idx <- values == "null"
  if (length(idx) == 0) return(values)
  values[!idx]
}
