#' Read STICS observation or simulation files (.obs or mod_s)
#'
#' @description Read STICS observation or simulation files from a JavaStics workspace and store data into a list per usm.
#' Used by `get_obs()` and `get_sim()`. Operate first computation and then call `get_file_()`.
#'
#' @param workspace      Path of a JavaStics workspace, or a vector of.
#' @param usm_name       Vector of usms to read (optional, used to filter usms)
#' @param var_list   vector of output variables names to filter (optional, see `get_var_info()` to get the names of the variables)
#' @param dates_list list of dates to filter (optional, should be a POSIX date)
#' @param usms_filepath  Path of the usms file (optional)
#' @param javastics_path JavaStics installation path (optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
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
                    usm_name = NULL,
                    var_list = NULL,
                    dates_list = NULL,
                    usms_filepath = NULL,
                    javastics_path = NULL,
                    verbose = TRUE,
                    type = c("sim","obs")){

  type = match.arg(type, c("sim","obs"), several.ok = FALSE)

  usms_path <- NULL

  # Try absolute path here (if it is, read the file only once, and pass its content)
  if (!is.null(usms_filepath)) {
    usms_path <- normalizePath(usms_filepath, mustWork = FALSE)
  # For the moment searching the usms.xml file in the workspace dir
  # is inactivated, before doing tests on performances.
  # TODO: test and uncomment else condition !
  # } else {
  #   usms_path <-
  #     normalizePath(file.path(workspace, "usms.xml"), mustWork = FALSE)
  }

  # Not keeping usms_filepath if does not exist
  if (!is.null(usms_path) && !file.exists(usms_path)) {
    warning(usms_path, ": file does not exist !")
    usms_path <- NULL
  }

  # Extracting data for a vector of workspace
  res = unlist(lapply(workspace,function(x){
    get_file_(workspace = x,
              usm_name = usm_name,
              usms_filepath = usms_path,
              var_list = var_list,
              dates_list = dates_list,
              javastics_path = javastics_path,
              verbose = verbose,
              type = type )
  }), recursive = FALSE)

  # Manage duplicated list names ?
  # TODO: is this usefull ?
  # If same usms exist in different folders, with different parameterization
  # this must be taken into account and rename duplicates instead of
  # deleting them !
  # res = res[!duplicated(names(res))]

  return(res)


  #if (!is.null(usms_filepath)) {
  # if(type == "sim"){
  #   file_pattern = "^mod_s"
  # }else{
  #   file_pattern = "\\.obs$"
  # }
  #
  # sim_name = get_file_from_usms(workspace = NULL, usms_path = usms_path, type = type, usm_name = usm_name)
  #
  # workspace_files = list.files(workspace, recursive = TRUE)
  #
  # sim_files = workspace_files[grepl(file_pattern,workspace_files)]
  #
  # # sim_name actually found in the folders:
  # exist_files <- unlist(lapply(sim_name, function(x) all(x %in% sim_files)))
  # sim_name = sim_name[exist_files]
  #
  # # Getting plant names, if javastics_path or workspace path contains
  # # a plant directory
  #
  # plant_names <- lapply(workspace,function(x){
  #   get_plant_name(x, usms_path, names(sim_name), javastics_path, verbose= FALSE)
  # })
  #
  # plant_names = unlist(plant_names, recursive = FALSE)
  #
  # plant_names = plant_names[!duplicated(names(plant_names))]
  # # names(plant_names) = names(sim_name)
  #
  # res = mapply(function(x,y,z){
  #   # In the case we give the usms.xml file as an absolute path, and we are working
  #   # with intercrops, the mapply would work on both files independently
  #   # and get_file_int do not see it is an intercrop. So we put them in a list
  #   # instead:
  #   several = FALSE
  #
  #   get_file_(workspace = y,
  #             usm_name = usm_name,
  #             usms_filepath = usms_filepath,
  #             var_list = var_list,
  #             dates_list = dates_list,
  #             javastics_path = javastics_path,
  #             verbose = verbose,
  #             file_name = x,
  #             plant_names = z,
  #             type = type, several = several)
  # },sim_name,workspace,plant_names)
  # names(res) = names(sim_name)
  #}else{
  # res = unlist(lapply(workspace,function(x){
  #   get_file_(workspace = x,
  #             usm_name = usm_name,
  #             usms_filepath = usms_filepath,
  #             var_list = var_list,
  #             dates_list = dates_list,
  #             javastics_path = javastics_path,
  #             verbose = verbose, type = type,
  #             several = TRUE)
  # }), recursive = FALSE)
  #}

  #res
}

#' Read STICS observation or simulation files (.obs or mod_s)
#'
#' @description Read STICS observation or simulation files from a JavaStics workspace and store data into a list per usm.
#' Used by `get_obs()` and `get_sim()`.
#'
#' @param workspace      Path of a JavaStics workspace
#' @param usm_name       Vector of usms to read (optional, used to filter usms)
#' @param usms_filepath  Path of the usms file (optional)
#' @param var_list   vector of output variables names to filter (optional, see `get_var_info()` to get the names of the variables)
#' @param dates_list list of dates to filter (optional, should be a POSIX date)
#' @param javastics_path JavaStics installation path (optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
# @param file_name      The names of the files to read (optional). read from `usms.xml` file if provided, or using file names
#' in the workspace directly
# @param plant_names    The plants names associated to the files (optional). If not provided, read `usms.xml`, or use
#'  default values if not available.
#' @param type          The type of file to read, either "obs" or "sim".
# @param several Is the function to be applied to eventually several workspace, file_name or plant_names?
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
                      usms_filepath = NULL,
                      var_list=NULL,
                      dates_list=NULL,
                      javastics_path = NULL,
                      verbose = TRUE,
                      #file_name = NULL,
                      #plant_names = NULL,
                      type = c("sim","obs")) {#,
                      #several){


  # TODO: add checking dates_list format, or apply
  # dates_list <- as.POSIXct(dates_list, tz="UTC", format = "%Y-%m-%d")

  type = match.arg(type, c("sim","obs"), several.ok = FALSE)
  if(type == "sim"){
    file_pattern = "^mod_s"
  }else{
    file_pattern = "\\.obs$"
  }

  # Getting files list from workspace vector
  workspace_files <- list.files(pattern = file_pattern, path = workspace)
  # If no files found, and usms name are given,
  # trying to find sub-directories named with usms names
  if (!length(workspace_files) && !is.null(usm_name)) {
    workspace <- file.path(workspace, usm_name)
    workspace_files <- as.list(list.files(pattern = file_pattern, path = workspace))
  }

  # No sim/obs file found
  if(!length(workspace_files)) {
    warning("Not any ", type, " file detected in workspace", workspace)
    return()
  }


  # No usms file path is given
  if(!is.null(usms_filepath) ){

    # In the get_file_from_usms the usms are filtered against
    # usm_name
    file_name <- get_file_from_usms(workspace = workspace,
                                    usms_path = usms_filepath,
                                    type = type,
                                    usm_name = usm_name)

    # Filtering existing files in file_name list
    exist_files <- unlist(lapply(file_name, function(x) all(x %in% workspace_files)))
    file_name = file_name[exist_files]

    # Exiting: not any existing files
    if (!length(file_name)) return()

    # Getting plant names, if javastics_path or workspace path contains
    # a plant directory, otherwise setting plant name to plant file name as a default.
    usms <- names(file_name)
    plant_names <- get_plant_name(workspace, usms_filepath, usms, javastics_path, verbose)
  }

  # The user did not provide any usms file path, so using the names of the .sti files
  # as information.
  if(is.null(usms_filepath) ) {

    # Getting sim/obs files list from directory
    file_name = parse_mixed_file(file_names = as.list(workspace_files), type = type)
    usms = names(file_name)

    # Selecting using usm_name
    if(!is.null(usm_name)) {
      usms <- intersect(usms, usm_name)
      # Not any matching names
      if (!length(usms)) return()
      file_name <- file_name[usms]
    }

    # Calculating plant ids
    plant_names = lapply(file_name, function(x){
      if(length(x)>1){
        c("plant_1", "plant_2")
      }else{
        c("plant_1")
      }
    })
  }

  # Getting sim/obs data list
  sim_list <- mapply(function(dirpath,filename,p_name){
    get_file_one(dirpath, filename, p_name,verbose, dates_list,var_list)
  },dirpath = workspace, filename = file_name, p_name = plant_names,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)

  names(sim_list) <- names(file_name)

  return(sim_list)
}


#' Get file for one workspace / file_name / plant_name
#'
#' Get a simulation or observation file for one situation at a time,
#' for sole or intercrop
#'
#' @param dirpath Path of a JavaStics workspace
#' @param filename File name(s)
#' @param p_name Plant name(s)
#' @param verbose Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#' @param dates_list list of dates to filter (optional, should be a POSIX date)
#' @param var_list vector of output variables names to filter (optional, see `get_var_info()` to get the names of the variables)
#'
#'
#' @return the obs or simulation output
#' @keywords internal
#'
get_file_one = function(dirpath, filename, p_name,
                        verbose, dates_list, var_list){
  out =
    get_file_int(dirpath, filename, p_name, verbose = verbose) %>%
    dplyr::select_if(function(x){any(!is.na(x))})

  # Filtering
  # Filtering Date on dates_list (format Posixct)
  if(!is.null(dates_list) & "Date" %in% names(out)){
    out <-
      out%>%
      dplyr::filter(.data$Date %in% dates_list)
  }

  # selecting variables columns
  if(!is.null(var_list)){
    # Managing output columns according to out content
    out_cols <- var_to_col_names(var_list)
    time_idx <- names(out) %in% c("ian", "mo","jo", "jul")
    time_elts <- names(out)[time_idx]
    out_cols <- c(time_elts, out_cols)
    if("cum_jul" %in% names(out)) out_cols <- c("cum_jul", out_cols)
    if ("Date" %in% names(out)) out_cols <- c("Date", out_cols)
    if ("Plant" %in% names(out)) out_cols <- c(out_cols, "Plant")
    out <-
      out%>%
      dplyr::select(dplyr::one_of(out_cols))
  }



  if(length(p_name) > 1){
    out$Dominance = "Principal"
    out$Dominance[out$Plant == p_name[2]] = "Associated"
  }
  out
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
      stop(usm_name,": do(es) not match usms")
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
  # files_exist <- unlist(lapply(file_name, function(x) all(file.exists(file.path(workspace,x)))))
  # Using now possible multiple workspaces
  files_exist  <- mapply(function(dirpath,filename){
    all(file.exists(file.path(dirpath, filename)))
  },dirpath = workspace, filename = file_name)

  file_name <- file_name[files_exist]

  return(file_name)
}


#' Get mixed file names
#'
#' Get mixed observation or simulation files by name
#'
#' @param file_names A list of files
#' @param type      The type of file to read, either "obs" or "sim".
#' @note The function use the obs/sim files names to retrieve the usm name.
#' So each obs file should be named with the usm name, followed by a or p
#' at the end in the case of associated crops.
#'
#' @return A list of observation or simulation files associated to their usm name. The mixed
#'  crops are always returned as c("Principal","Associated").
#'
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
