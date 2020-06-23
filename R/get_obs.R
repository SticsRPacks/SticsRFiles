#' Read STICS observation files (*.obs)
#'
#' @description Read STICS observation files from a JavaStics workspace and store data into a list per usm
#'
#' @param workspace Path of a JavaStics workspace, or a vector of (recursive call).
#' @param usm_name      Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  Name of the usm file (optional, default to the standard `usms.xml`)
#' @param javastics_path JavaStics installation path (Optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#'
#' @details The `.obs` files names should match the name of the USM, e.g. for a usm called "banana",
#' the `.obs` file should be named `banana.obs`. For intercrops, the name should be suffixed by "p" for
#' the principal and "a" for the associated plant.
#' If `usm_name` is not specified (or equal to `NULL`), the
#' function read the obs files from all usms listed in `usms_filename` and present in the `workspace` folder.
#'
#' @return A list of `data.frame`s with observations. The function always returns a list of `data.frame`s for the corresponding usm,
#' even if an `.obs` file is missing (in this case it returns an empty `data.frame`).
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
get_obs= function(workspace=getwd(), usm_name=NULL, usms_filename="usms.xml",javastics_path = NULL,verbose=TRUE){

  # For a list of folders recurive call
  if(length(workspace) > 1){
    obs_list <- lapply(workspace, function(x) get_obs(x,usm_name, usms_filename))
    n <- unlist(lapply(obs_list, function(x) names(x[1])))
    obs_list <- lapply(obs_list, function(x) x[[1]])
    names(obs_list) <- n
    return(obs_list)
  }

  # getting usms names from the usms.xml file:
  usms= get_usms_list(usm_path = file.path(workspace,usms_filename))

  # Filtering USMs if required:
  if(!is.null(usm_name)){
    usm_exist <- usm_name %in% usms

    # Some provided usms are not available:
    if(!all(usm_exist)){
      if(verbose){
        cli::cli_alert_danger("The usm{?s} {.val {usm_name[!usm_exist]}} d{?oes/o} not exist in the workspace!")
        cli::cli_alert_info("Usm{?s} found in the workspace: {.val {usms}}")
      }
      stop("usm_name do not match usms")
    }
    usms= usm_name
  }

  # Intercopping ?
  mixed= get_plants_nb(file.path(workspace,usms_filename))[usms] > 1

  # Extracting expected observation file names:
  obs_name= vector(mode= "list", length = length(usms))
  names(obs_name)= usms
  obs_name[!mixed]= paste0(usms[!mixed],".obs")
  obs_name[mixed]= lapply(usms[mixed], function(x){paste0(x,c("p","a"),".obs")})

  file_exist= lapply(obs_name, function(x)file.exists(file.path(workspace,x)))

  # Getting plant names:
  plant_names= get_plant_name(workspace, usm_name, usms_filename, javastics_path, verbose)

  obs_list= mapply(function(dirpath,filename,mixed,read_it,p_name){
    if(all(read_it)){
      get_obs_int(dirpath,filename,p_name)%>%
        dplyr::select_if(function(x){any(!is.na(x))}) # Remove variables with only NAs.
    }else{
      data.frame()
    }
    },dirpath= workspace, filename= obs_name, read_it= file_exist,
    p_name= plant_names, SIMPLIFY = FALSE)
  names(obs_list)= usms
  attr(obs_list, "class")= "stics_observation"
  return(obs_list)
}



