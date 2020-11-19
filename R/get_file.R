#' Read STICS observation or simulation files (.obs or mod_s)
#'
#' @description Read STICS observation or simulation files from a JavaStics workspace and store data into a list per usm.
#' Used by `get_obs()` and `get_daily_results()`. Operate first computation and then call `get_file_()`.
#'
#' @param workspace      Path of a JavaStics workspace, or a vector of (recursive call).
#' @param usm_name       Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  Name of the usm file (optional)
#' @param var_list   vector of output variables names to filter (optional, see `get_var_info()` to get the names of the variables)
#' @param doy_list   vector of cumulative DOYs to filter (optional)
#' @param dates_list list of dates to filter (optional, should be a POSIX date)
#' @param javastics_path JavaStics installation path (optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#' @param file_name      The names of the files to read (optional). read from `usms.xml` file if provided, or using file names
#' in the workspace directly
#' @param plant_names    The plants names associated to the files (optional). If not provided, read `usms.xml`, or use
#'  default values if not available.
#' @param type          The type of file to read, either "obs" or "sim".
#'
#' @details The `.obs` files names should match USMs mames, e.g. for a usm called "banana",
#' the `.obs` file should be named `banana.obs`. For intercrops, the name should be suffixed by "p" for
#' the principal and "a" for the associated plant.
#'
#' @return A named list of `data.frame`s with observations or simulation data. The list elements are named after
#' the usms names.
#'
#' @keywords internal
#'
get_file = function(workspace,
                    usm_name=NULL,
                    var_list=NULL,
                    doy_list=NULL,
                    dates_list=NULL,
                    usms_filename= NULL,
                    javastics_path = NULL,
                    verbose= TRUE,
                    type = c("sim","obs")){

  type = match.arg(type, c("sim","obs"), several.ok = FALSE)

  if(!is.null(usms_filename) &&
     file.exists(normalizePath(usms_filename, mustWork = FALSE))){
    if(type == "sim"){
      file_pattern = "^mod_s"
    }else{
      file_pattern = "\\.obs$"
    }

    # Try absolute path here (if it is, read the file only once, and pass its content)
    usms_path <- normalizePath(usms_filename, mustWork = FALSE)

    sim_name = get_file_from_usms(workspace = NULL, usms_path = usms_path, type = type, usm_name = usm_name)

    workspace_files = list.files(workspace, recursive = TRUE)

    sim_files = workspace_files[grepl(file_pattern,workspace_files)]

    # sim_name actually found in the folders:
    sim_name = sim_name[sim_name %in% sim_files]

    # Getting plant names, if javastics_path or workspace path contains
    # a plant directory

    plant_names <- lapply(workspace,function(x){
      get_plant_name(x, names(sim_name), usms_path, javastics_path, verbose= FALSE)
    })

    plant_names = unlist(plant_names, recursive = FALSE)

    plant_names = plant_names[!duplicated(names(plant_names))]
    # names(plant_names) = names(sim_name)

    res = mapply(function(x,y,z){
      get_file_(workspace = y,
                usm_name = usm_name,
                usms_filename = usms_filename,
                var_list = var_list,
                doy_list = doy_list,
                dates_list = dates_list,
                javastics_path = javastics_path,
                verbose = verbose,
                file_name = x,
                plant_names = z,
                type = type)
    },sim_name,workspace,plant_names)
  }else{
    res = unlist(lapply(workspace,function(x){
      get_file_(workspace = x,
                usm_name = usm_name,
                usms_filename = usms_filename,
                var_list = var_list,
                doy_list = doy_list,
                dates_list = dates_list,
                javastics_path = javastics_path,
                verbose = verbose, type = type)
    }), recursive = FALSE)
  }

  res
}

#' Read STICS observation or simulation files (.obs or mod_s)
#'
#' @description Read STICS observation or simulation files from a JavaStics workspace and store data into a list per usm.
#' Used by `get_obs()` and `get_daily_results()`.
#'
#' @param workspace      Path of a JavaStics workspace, or a vector of (recursive call).
#' @param usm_name       Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  Name of the usm file (optional)
#' @param var_list   vector of output variables names to filter (optional, see `get_var_info()` to get the names of the variables)
#' @param doy_list   vector of cumulative DOYs to filter (optional)
#' @param dates_list list of dates to filter (optional, should be a POSIX date)
#' @param javastics_path JavaStics installation path (optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#' @param file_name      The names of the files to read (optional). read from `usms.xml` file if provided, or using file names
#' in the workspace directly
#' @param plant_names    The plants names associated to the files (optional). If not provided, read `usms.xml`, or use
#'  default values if not available.
#' @param type          The type of file to read, either "obs" or "sim".
#'
#' @details The `.obs` files names should match USMs mames, e.g. for a usm called "banana",
#' the `.obs` file should be named `banana.obs`. For intercrops, the name should be suffixed by "p" for
#' the principal and "a" for the associated plant.
#'
#' @return A named list of `data.frame`s with observations or simulation data. The list elements are named after
#' the usms names.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
#'
get_file_ <- function(workspace = getwd(),
                      usm_name = NULL,
                      usms_filename = NULL,
                      var_list=NULL,
                      doy_list=NULL,
                      dates_list=NULL,
                      javastics_path = NULL,
                      verbose = TRUE,
                      file_name = NULL,
                      plant_names = NULL,
                      type = c("sim","obs")){

  type = match.arg(type, c("sim","obs"), several.ok = FALSE)
  if(type == "sim"){
    file_pattern = "^mod_s"
  }else{
    file_pattern = "\\.obs$"
  }
  # Getting sim files list from usms.xml file or obs files found in workspace

  # get_daily_results(), the calling function did not found the usms.xml file given using
  # an absolute path, but the user gives a usms_filename, so it must be relative to
  # the workspace
  if(!is.null(usms_filename) && is.null(file_name) && is.null(plant_names)){

    # Try relative path first
    usms_path <- normalizePath(file.path(workspace,usms_filename), mustWork = FALSE)

    if(!file.exists(usms_path)){
      stop(usms_filename, " not found in workspace or as an absolute path")
    }

    file_name <- get_file_from_usms(workspace = workspace,
                                    usms_path = usms_path,
                                    type = type,
                                    usm_name = usm_name)

    # Getting plant names, if javastics_path or workspace path contains
    # a plant directory

    usms <- names(file_name)
    plant_names <- get_plant_name(workspace, usms, usms_path, javastics_path, verbose)
  }

  # The user did not provide any usms_filename, so using the names of the .sti files
  # as information.
  if(is.null(usms_filename) && is.null(file_name) && is.null(plant_names)){
    # Getting obs files list from directory
    file_name <- as.list(list.files(pattern = file_pattern, path = workspace, full.names = FALSE))
    file_name = parse_mixed_file(file_names = file_name, type = type)
    usms = names(file_name)
    # No obs file found
    if(!length(file_name)) return()

    # Selecting using usm_name
    if(!is.null(usm_name)) {
      usms <- intersect(usms, usm_name)

      # Not any matching manes
      if (!length(usms)) return()
      file_name <- file_name[usms]

    }

    plant_names = lapply(file_name, function(x){
      if(length(x)>1){
        c("plant_1", "plant_2")
      }else{
        c("plant_1")
      }
    })
  }


  # Getting obs data list, removing variables with only NAs
  sim_list <- mapply(function(dirpath,filename,p_name){
    out =
      get_file_int(dirpath, filename, p_name, verbose = verbose) %>%
      dplyr::select_if(function(x){any(!is.na(x))})

    # selecting variables columns
    if(!is.null(var_list)){
      out <-
        out%>%
        dplyr::select(c("ian", "mo","jo", "jul"),dplyr::one_of(var_to_col_names(var_list)))
    }

    # Filtering:

    # filtering dates on DOY (using cum_jul)
    if(!is.null(doy_list)){
      out <-
        out%>%
        dplyr::filter(.data$cum_jul %in% doy_list)
    }

    # filtering dates on Date
    if(!is.null(dates_list)){
      out <-
        out%>%
        dplyr::filter(.data$Date %in% dates_list)
    }

    if(length(p_name) > 1){
      out$Dominance = "Principal"
      out$Dominance[out$Plant == p_name[2]] = "Associated"
    }else{
      out = out[,- grep("Plant",colnames(out))]
    }
    out
  },dirpath = workspace, filename = file_name, p_name = plant_names, SIMPLIFY = FALSE)

  names(sim_list) <- names(file_name)

  return(sim_list)
}


get_file_from_usms <- function(workspace,
                               usms_path,
                               type = c("sim","obs"),
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
  file_name <- vector(mode = "list", length = length(usms))
  names(file_name) <- usms

  if(type == "sim"){
    file_name[!mixed] <- paste0("mod_s",usms[!mixed],".sti")
    file_name[mixed] <- lapply(usms[mixed], function(x){paste0("mod_s",c("p","a"),x,".sti")})
  }else{
    file_name[!mixed] <- paste0(usms[!mixed],".obs")
    file_name[mixed] <- lapply(usms[mixed], function(x){paste0(x,c("p","a"),".obs")})
  }

  # Filtering with all files exist
  files_exist <- unlist(lapply(file_name, function(x) all(file.exists(file.path(workspace,x)))))
  file_name <- file_name[files_exist]

  return(file_name)
}


#' Get mixed file names
#'
#' Get mixed observation or simulation files by name
#'
#' @note The function use the obs/sim files names to retrieve the usm name.
#' So each obs file should be named with the usm name, followed by a or p
#' at the end in the case of associated crops.
#'
#' @return A list of observation or simulation files associated to their usm name. The mixed
#'  crops are always returned as c("Principal","Associated").
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' parse_mixed_file(list("banana.obs", "IC_banana_sorghuma.obs", "IC_banana_sorghump.obs"),
#'                   type = "obs")
#'
#' # Simulations with usm names starting with "a", with or without intercropping:
#' file_names = list("mod_sauzevilleSC_Wheat_Wheat_2005-2006_N0.sti",
#' "mod_saIC_Wheat_Wheat_2005-2006_N0.sti", "mod_spIC_Wheat_Wheat_2005-2006_N0.sti")
#'
#' parse_mixed_file(file_names, type = "sim")
#'
#' }
#'
parse_mixed_file = function(file_names, type = c("sim","obs")){

  type = match.arg(type, c("sim","obs"), several.ok = FALSE)

  if(type == "sim"){
    usm_pattern = "^(mod_s)|(\\.sti)$"
    mixed_pattern = "^(mod_s(a|p))|(\\.sti)$"
    associated_pattern = "^mod_sa"
  }else{
    usm_pattern = "\\.obs$"
    mixed_pattern = "((a|p)\\.obs)$"
    associated_pattern = "a.obs$"
  }

  usm_names= gsub(pattern = usm_pattern, replacement = "",x = file_names)
  names(file_names) = usm_names

  is_potential_mixed = grepl(mixed_pattern,file_names)
  usm_name_potential_mixed = gsub(pattern = mixed_pattern, replacement = "",x = file_names)
  potential_mixed = usm_name_potential_mixed[is_potential_mixed]

  file_names2 = file_names

  mixed_and_not_duplicated = seq_along(file_names)[is_potential_mixed][!duplicated(potential_mixed)]

  for(i in mixed_and_not_duplicated){
    mixed = which(usm_name_potential_mixed[i] == usm_name_potential_mixed & is_potential_mixed)
    # [!duplicated(potential_mixed)]
    if(length(mixed) > 1){
      mixed_names = unlist(file_names[mixed])
      associated_index = grep(associated_pattern,mixed_names)
      file_names2[[i]] = c(mixed_names[-associated_index], mixed_names[associated_index])
      names(file_names2)[i] = usm_name_potential_mixed[i]
    }else{
      # Here we thougth it was mixed, but it really is not because we did not found another
      # associated file with the same name modulo "a" or "p"
      names(file_names2)[i] = gsub(pattern = usm_pattern, replacement = "",x = file_names2[i])
    }
  }
  file_names2[c(which(!is_potential_mixed),mixed_and_not_duplicated)]
}
