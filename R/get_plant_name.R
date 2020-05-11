
#' Get plant names
#'
#' @description Get the plant name (and file name) for each usm in a workspace
#'
#' @param workspace      Path of a JavaStics workspace, or a vector of (recursive call).
#' @param usm_name      Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  Name of the usm file (optional, default to the standard `usms.xml`)
#' @param javastics_path JavaStics installation path (Optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#'
#'
#' @return A list of plant names for each usm with one value for sole crop and two values for intercrop (principal, associated).
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' path <- get_examples_path( file_type = "xml")
#'
#' # Get plant names for all usms (no javastics path, so only the file name is returned):
#' get_plant_name(path)
#' # Get plant names only for banana:
#' get_plant_name(path, "banana")
#'
#' # Get plant names for banana with a javastics path, so the plant name is returned:
#' get_plant_name(path)
#' # NB: if the plant folder is in the workspace, no need to provide javastics_path
#'
#' }
#'
get_plant_name= function(workspace,usm_name=NULL,usms_filename= "usms.xml",javastics_path = NULL, verbose=TRUE){

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

  nb_plant= get_plants_nb(file.path(workspace,usms_filename))[usms]

  # Get plant file(s) for each usm:
  plant_xml= try(
    mapply(function(x,y){
      get_param_xml(xml_file = file.path(workspace,usms_filename), param_name = "fplt",
                    select = "usm", x)[[1]][1:y]
    },x= usms, y= nb_plant)
  )

  if(inherits(plant_xml,"try-error")){
    plant_xml= vector(mode = "list", length = length(usms))
    plant_xml[nb_plant==1]= "plant_1"
    plant_xml[nb_plant>1]= c("plant_1","plant_2")
    names(plant_xml)= usms
    if(verbose) cli::cli_alert_warning("Error reading usms file, using dummy plant names")
    return(plant_xml)
  }

  if(is.null(javastics_path)){
    plt_path <- file.path(workspace, "plant")
    if(!dir.exists(plt_path)){
      cli::cli_alert_warning("plant folder not found in the workspace, please add {.code javastics_path} to use real plant names from javaStics.")
      return(plant_xml)
    }
  }else{
    plt_path <- try(normalizePath(file.path(javastics_path, "plant")))
  }

  plant_names=
    try(
      lapply(plant_xml, function(x){
        unlist(get_param_xml(xml_file = normalizePath(file.path(plt_path,x)),
                             param_name = "codeplante"))
      })
    )

  if(inherits(plant_names,"try-error")){
    plant_names= plant_xml
    if(verbose) cli::cli_alert_warning("Error reading plant names, using plant file names for the output instead")
  }

  return(plant_names)
}
