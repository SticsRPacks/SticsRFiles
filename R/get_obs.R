#' Read STICS observation files (*.obs)
#'
#' @description Read STICS observation files from a JavaStics workspace and store data into a list per usm
#'
#' @param workspace_path Path of a JavaStics workspace, or a vector of (recursive call).
#' @param usms_list      Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  Name of the usm file (optional, default to the standard `usms.xml`)
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#'
#' @details The `.obs` files names should match the name of the USM, e.g. for a usm called "banana",
#' the `.obs` file should be named `banana.obs`. For intercrops, the name should be suffixed by "p" for
#' the principal and "a" for the associated plant.
#' If `usms_list` is not specified (or equal to `NULL`), the
#' function read the obs files from all usms listed in `usms_filename` and present in the `workspace_path` folder.
#'
#' @return A list of `data.frame`s with observations. The function always returns a list of `data.frame`s for the corresponding usm,
#' even if an `.obs` file is missing (in this case it returns a `data.frame` with 0 rows and 0 columns.
#'
#' @examples
#' \dontrun{
#' path <- system.file(file.path("extdata","obs","V9.0"), package = "SticsRFiles")
#'
#' # Get observations for all usms, but only banana has observations:
#' Meas <- get_obs(path)
#' # Get observations only for banana:
#' Meas_banana <- get_obs(path, "banana")
#' }
#'
#' @export
#'
get_obs= function(workspace_path=getwd(), usms_list=NULL, usms_filename="usms.xml",verbose=TRUE){

  # For a list of folders recurive call
  if(length(workspace_path) > 1){
    obs_list <- lapply(workspace_path, function(x) get_obs(x,usms_list, usms_filename))
    n <- unlist(lapply(obs_list, function(x) names(x[1])))
    obs_list <- lapply(obs_list, function(x) x[[1]])
    names(obs_list) <- n
    return(obs_list)
  }

  # getting usms names from the usms.xml file:
  usms= get_usms_list(usm_path = file.path(workspace_path,usms_filename))

  # Filtering USMs if required:
  if(!is.null(usms_list)){
    usm_exist <- usms_list %in% usms

    # Some provided usms are not available:
    if(!all(usm_exist)){
      if(verbose){
        cli::cli_alert_danger("The usm{?s} {.val {usms_list[!usm_exist]}} d{?oes/o} not exist in the workspace!")
        cli::cli_alert_info("Usm{?s} found in the workspace: {.val {usms}}")
      }
      stop("usms_list do not match usms")
    }
    usms= usms_list
  }

  nb_plants= get_plants_nb(file.path(workspace_path,usms_filename))[usms]
  is_mixed= nb_plants > 1
  # Extracting expected observation file names:
  obs_name= vector(mode= "list", length = length(usms))
  names(obs_name)= usms
  obs_name[!is_mixed]= paste0(usms[!is_mixed],".obs")
  obs_name[is_mixed]= lapply(usms[is_mixed], function(x){paste0(x,c("p","a"),".obs")})

  file_exist= lapply(obs_name, function(x)file.exists(file.path(workspace_path,x)))

  obs_list= mapply(function(dirpath,filename,mixed,read_it){
    if(all(read_it)){
      get_obs_int(dirpath,filename,mixed)%>%
        dplyr::select_if(function(x){any(!is.na(x))}) # Remove variables with only NAs.
    }else{
      data.frame()
    }
    },dirpath= workspace_path, filename= obs_name, mixed= is_mixed, read_it= file_exist, SIMPLIFY = FALSE)
  names(obs_list)= usms

  return(obs_list)
}
