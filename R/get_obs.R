#' Read STICS observation files (*.obs)
#'
#' @description Read STICS observation files from a JavaStics workspace and store data into a list per usm
#'
#' @param workspace Path of a JavaStics workspace, or a vector of (recursive call).
#' @param usm_name      Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  Name of the usm file (optional)
#' @param javastics_path JavaStics installation path (optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#'
#' @details The `.obs` files names should match USMs mames, e.g. for a usm called "banana",
#' the `.obs` file should be named `banana.obs`. For intercrops, the name should be suffixed by "p" for
#' the principal and "a" for the associated plant.
#' If `usm_name` is not specified (or equal to `NULL`), the
#' function reads the observation files from all usms listed in `usms_filename` and present in the `workspace` folder(s).
#' Or if `usms_filename` is not specified, all the .obs files are loaded from the `workspace` folder(s).
#'
#'
#' @return A named list of `data.frame`s with observations data. Names of the list elements may be either usms names,
#' or observation file name if the `usms_filename` is not specified.
#'
#'
#' @examples
#' \dontrun{
#' path <- file.path(get_examples_path( file_type = "obs"))
#'
#' # Get observations for all usms, but only banana has observations:
#' Meas <- get_obs(path)
#' # Get observations only for banana:
#' Meas_banana <- get_obs(path, "banana")
#'
#' # Get oservations with real plant names when plant folder is not in the workspace:
#' get_obs(path, "banana", javastics_path= "path/to/javastics")
#' }
#'
#' @export
#'
get_obs <- function(workspace = getwd(),
                    usm_name = NULL,
                    usms_filename = NULL,
                    javastics_path = NULL,
                    verbose = TRUE){

  # For a list of folders recurive call
  if(length(workspace) > 1){
    obs_list <- unlist(lapply(workspace,
                              function(x) get_obs(x,
                                                  usm_name = usm_name,
                                                  usms_filename = usms_filename,
                                                  javastics_path = javastics_path,
                                                  verbose = verbose)),
                       recursive = FALSE)
    return(obs_list)
  }

  # Getting obs files list from usms.xml file or obs files found in workspace

  # Getting existing obs files list using usms.xml

  if (! base::is.null(usms_filename)) {

    usms_path <- file.path(workspace,usms_filename)

    if(! file.exists(usms_path)){
      stop(usms_filename, ": does not exist in directory ",workspace)
    }


    obs_name <- get_obs_from_usms(workspace = workspace,
                                  usms_path = usms_path,
                                  usms_filename = usms_filename,
                                  usm_name = usm_name)

    # Getting plant names, if javastics_path or workspace path contains
    # a plant directory
    #
    usms <- names(obs_name)
    plant_names <- get_plant_name(workspace, usms, usms_filename, javastics_path, verbose)

  } else {
    # Getting obs files list from directory
    obs_name <- as.list(list.files(pattern = "\\.obs$", path = workspace, full.names = FALSE))

    # No obs file found
    if (!length(obs_name)) return()

    usms <- gsub(pattern = "\\.obs$", replacement = "",x = obs_name)
    names(obs_name) <- usms

    # Selecting using usm_name
    if (!base::is.null(usm_name)) {
      usms <- intersect(usms, usm_name)

      # Not any matching manes
      if (!length(usms)) return()
      obs_name <- obs_name[usms]

    }
    plant_names <- rep("NA",length(obs_name))
  }

  # Getting obs data list, removing variables with only NAs
  obs_list <- mapply(function(dirpath,filename,p_name){
    get_obs_int(dirpath, filename, p_name, verbose = verbose) %>%
      dplyr::select_if(function(x){any(!is.na(x))})
  },dirpath = workspace, filename = obs_name, p_name = plant_names, SIMPLIFY = FALSE)

  names(obs_list) <- usms

  # Not used elsewhere, commented for the moment !
  # attr(obs_list, "class") <- "stics_observation"
  return(obs_list)
}



get_obs_from_usms <- function(workspace,
                              usms_path,
                              usms_filename,
                              usm_name = NULL,
                              verbose = TRUE) {

  # Getting usms names from the usms.xml file
  usms <- get_usms_list(usm_path = file.path(usms_path))

  # Filtering USMs if required:
  if (!is.null(usm_name)){
    usm_exist <- usm_name %in% usms

    # Some provided usms are not available:
    if (!all(usm_exist)){
      if (verbose){
        cli::cli_alert_danger("The usm{?s} {.val {usm_name[!usm_exist]}} d{?oes/o} not exist in the workspace!")
        cli::cli_alert_info("Usm{?s} found in the workspace: {.val {usms}}")
      }
      stop("usm_name do not match usms")
    }
    usms <- usm_name
  }

  # Intercopping
  mixed <- get_plants_nb(file.path(workspace,usms_filename))[usms] > 1

  # Extracting expected observation file names:
  obs_name <- vector(mode = "list", length = length(usms))
  names(obs_name) <- usms
  obs_name[!mixed] <- paste0(usms[!mixed],".obs")
  obs_name[mixed] <- lapply(usms[mixed], function(x){paste0(x,c("p","a"),".obs")})

  # Filtering with all files exist
  files_exist <- unlist(lapply(obs_name, function(x) all(file.exists(file.path(workspace,x)))))
  obs_name <- obs_name[files_exist]

  return(obs_name)

}





