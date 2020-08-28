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
#' path <- file.path(get_examples_path(file_type = "obs"),"mixed")
#'
#' # Get observations for all usms, but only banana has observations:
#' Meas <- get_obs(path)
#' # Get observations only for banana:
#' Meas_banana <- get_obs(path, "banana")
#'
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

  if(length(workspace> 1) && !is.null(usms_filename) &&
     file.exists(normalizePath(usms_filename, mustWork = FALSE))){
    # Try absolute path here (if it is, read the file only once, and pass its content)
    usms_path <- normalizePath(usms_filename, mustWork = FALSE)

    obs_name = get_obs_from_usms(workspace = NULL, usms_path = usms_path, usm_name = usm_name)

    workspace_files = list.files(workspace,recursive = TRUE)

    obs_files = workspace_files[grepl(".obs",workspace_files)]

    # obs_name actually found in the folders:
    obs_name = obs_name[obs_name %in% obs_files]

    # Getting plant names, if javastics_path or workspace path contains
    # a plant directory

    plant_names <- lapply(workspace,function(x){
      get_plant_name(x, names(obs_name), usms_path, javastics_path, verbose= FALSE)
    })

    plant_names = unlist(plant_names, recursive = FALSE)

    plant_names = plant_names[!duplicated(names(plant_names))]
    # names(plant_names) = names(obs_name)

    mapply(function(x,y,z){
      get_obs_(y,
               usm_name = usm_name,
               usms_filename = usms_filename,
               javastics_path = javastics_path,
               verbose = verbose,x,z)
    },obs_name,workspace,plant_names)
  }else{
    unlist(lapply(workspace,function(x){
      get_obs_(x,
               usm_name = usm_name,
               usms_filename = usms_filename,
               javastics_path = javastics_path,
               verbose = verbose)
    }), recursive = FALSE)
  }
}

#' @rdname get_obs
#' @keywords internal
get_obs_ <- function(workspace = getwd(),
                     usm_name = NULL,
                     usms_filename = NULL,
                     javastics_path = NULL,
                     verbose = TRUE,
                     obs_name = NULL,
                     plant_names = NULL){

  # Getting obs files list from usms.xml file or obs files found in workspace


  # get_obs(), the calling function did not found the usms.xml file given using
  # an absolute path, but the user gives a usms_filename, so it must be relative to
  # the workspace
  if(!is.null(usms_filename) && is.null(obs_name) && is.null(plant_names)){

    # Try relative path first
    usms_path <- normalizePath(file.path(workspace,usms_filename), mustWork = FALSE)

    if(!file.exists(usms_path)){
      stop(usms_filename, " not found in workspace or as an absolute path")
    }

    obs_name <- get_obs_from_usms(workspace = workspace,
                                  usms_path = usms_path,
                                  usm_name = usm_name)

    # Getting plant names, if javastics_path or workspace path contains
    # a plant directory

    usms <- names(obs_name)
    plant_names <- get_plant_name(workspace, usms, usms_path, javastics_path, verbose)
  }

  # The user did not provide any usms_filename, so using the names of the .obs files
  # as information.
  if(is.null(usms_filename) && is.null(obs_name) && is.null(plant_names)) {
    # Getting obs files list from directory
    obs_name <- as.list(list.files(pattern = "\\.obs$", path = workspace, full.names = FALSE))
    obs_name = parse_mixed_obs(obs_name)
    usms = names(obs_name)
    # No obs file found
    if (!length(obs_name)) return()

    # Selecting using usm_name
    if (!base::is.null(usm_name)) {
      usms <- intersect(usms, usm_name)

      # Not any matching manes
      if (!length(usms)) return()
      obs_name <- obs_name[usms]

    }


    plant_names = lapply(obs_name, function(x){
      if(length(x)>1){
        c("plant_1", "plant_2")
      }else{
        c("plant_1")
      }
    })

  }

  # Getting obs data list, removing variables with only NAs
  obs_list <- mapply(function(dirpath,filename,p_name){
    get_obs_int(dirpath, filename, p_name, verbose = verbose) %>%
      dplyr::select_if(function(x){any(!is.na(x))})
  },dirpath = workspace, filename = obs_name, p_name = plant_names, SIMPLIFY = FALSE)

  names(obs_list) <- names(obs_name)

  # Not used elsewhere, commented for the moment !
  # attr(obs_list, "class") <- "stics_observation"
  return(obs_list)
}



get_obs_from_usms <- function(workspace,
                              usms_path,
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
  mixed <- get_plants_nb(usms_path)[usms] > 1

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


#' Get mixed obs
#'
#' Get mixed observation files by name
#'
#' @return
#' @keywords internal
#'
#' @examples
#'
#' parse_mixed_obs(list("banana.obs", "IC_banana_sorghuma.obs", "IC_banana_sorghump.obs"))
#'
parse_mixed_obs = function(obs_names){

  usm_names= gsub(pattern = "\\.obs$", replacement = "",x = obs_names)
  names(obs_names) = usm_names

  is_potential_mixed = grepl("((a|p)\\.obs)$",obs_names)
  usm_name_potential_mixed = gsub(pattern = "((a|p)\\.obs)$", replacement = "",x = obs_names)
  potential_mixed = usm_name_potential_mixed[is_potential_mixed]

  obs_names2 = obs_names

  mixed_and_not_duplicated = seq_along(obs_names)[is_potential_mixed][!duplicated(potential_mixed)]

  for(i in mixed_and_not_duplicated){
    mixed = which(usm_name_potential_mixed[i] == usm_name_potential_mixed & is_potential_mixed)
    # [!duplicated(potential_mixed)]
    if(length(mixed) > 1){
      mixed_names = unlist(obs_names[mixed])
      associated_index = grep("a.obs$",mixed_names)
      obs_names2[[i]] = c(mixed_names[-associated_index], mixed_names[associated_index])
      names(obs_names2)[i] = usm_name_potential_mixed[i]
    }
  }

  obs_names2[c(which(!is_potential_mixed),mixed_and_not_duplicated)]
}
