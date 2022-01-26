#' @title Generating one or several usms directories from a javastics workspace
#' content
#'
#' @description The function creates sets of input files for one or multiple usms
#' from usms data stored in a JavaStics workspace. For multiple usms, sets will
#' be generated into individual folders named with usm names.. Observations
#' files will be also copied if they are named `[usm_name].obs`
#' For one usm, files will be generated either in the workspace directory
#' or in a subdirectory.
#'
#'
#' @param javastics_path Path of JavaStics installation directory
#' @param workspace_path Path of a JavaStics workspace (Optional), if not
#' provided the current workspace stored in JavaStics preferences will be used.
#' @param target_path The path of the directory where to create usms directories (Optional),
#' if not provided the JavaStics workspace will be used as root
#' @param usms_list List of usms to generate (Optional). If not provided, all
#' usms contained in workspace_path/usms.xml file will be generated.
#' @param verbose Logical value for displaying information  while running (TRUE) ot not (FALSE)
#' @param dir_per_usm_flag logical, TRUE if one want to create one directory per USM,
#' FALSE if USM files are generated in the target_path (only useful for usms_list of size one)
#' @param check_files Logical, TRUE to check if usms files exist, FALSE otherwise
#'
#' @return A list with named elements:
#' usms_path : created directories paths (for storing Stics input files),
#' files : generated files list (in JavaStics workspace origin),
#' copy_status : logical value vector, indicating if all files have been generated for each usm
#' obs_copy_status : logical value vector, indicating if observaion files have been
#' successfully copied in usms directories
#'
#' @examples
#' \dontrun{
#' javastics <- "/path/to/javastics"
#' javastics_workspace <- "/path/to/workspace"
#'
#' # For all usms
#' gen_usms_xml2txt(javastics, javastics_workspace)
#'
#' # For an usms list
#' usm_list <- c("usm1", "usm2")
#' gen_usms_xml2txt(javastics, javastics_workspace, usm_list)
#'
#' # For one usm
#' gen_usms_xml2txt(javastics, javastics_workspace, dir_per_usm_flag=F, "usm1")
#'
#' }
#'
#' @export
#'
#'

gen_usms_xml2txt <- function(javastics_path,
                             workspace_path = NULL,
                             target_path = NULL,
                             usms_list = c(),
                             verbose = TRUE,
                             dir_per_usm_flag=TRUE,
                             check_files = TRUE) {
  # overwrite = FALSE,parallelized = FALSE, exec_time = FALSE) {


  ################### TODO ######################################
  # TODO : for file overwriting right, activate obverwriting arg
  # and add overwriting management in the code !
  #
  # TODO : for parallel work add a copy of workspace_path
  # and calculate if at the beginning of the foreach loop !

  #library(doParallel)

  # parallel computing management
  # cores_nb <- 1
  # if ( parallelized ) {
  #   cores_nb <- 2
  # }
  #
  # cl <- parallel::makeCluster(cores_nb)
  # registerDoParallel(cl)
  #################################################################


  # checking javastics path
  SticsOnR:::check_java_path(javastics_path)
  start_wd= getwd()
  on.exit(setwd(start_wd))

  setwd(javastics_path)


  # Checking and getting JavaStics workspace path
  workspace_path <- SticsOnR:::check_java_workspace(javastics_path,workspace_path)
  if (base::is.null(workspace_path)) {
    return()
  }


  # Setting the javastics workspace as root directory for usms
  # directories to generate
  if (base::is.null(target_path)) target_path <- workspace_path

  # Creating target dir if not exists
  if (! dir.exists(target_path)) {
    dir.create(target_path)
  }

  # Retrieving usm names list from the usms.xml file
  full_usms_list = get_usms_list(usm_path = file.path(workspace_path,"usms.xml"))

  # Do some usms have lai forcing? If so, read it accordingly:
  lai_forcing = get_lai_forcing_xml(file.path(workspace_path,"usms.xml"))
  lai_file_path =
    file.path(workspace_path,
              get_param_xml(xml_file = file.path(workspace_path,"usms.xml"),
                            param = "flai")[[1]]$flai)

  dominance = get_param_xml(xml_file = file.path(workspace_path,"usms.xml"),
                            param = "dominance")[[1]]$dominance


  nbplantes = get_param_xml(xml_file = file.path(workspace_path,"usms.xml"),
                            param = "nbplantes")[[1]]$nbplantes

  flai_usms = vector(mode = "list", length = length(full_usms_list))
  names(flai_usms) = full_usms_list
  usm_index = 1 # This is equivalent of i, but tracks which usm we are doing in
  # the for loop below, because sometimes we have two lai files

  for(i in seq_along(lai_file_path)){
    if(dominance[i] == 1){
      flai_usms[[usm_index]] = lai_file_path[i]
    }else if(nbplantes[usm_index] == 2){
      # Here we provide the lai file for the second plant, but just if
      # nbplantes = 2 because it can still be parameterized or be =null when
      # nbplantes = 1 (but we don't want the 2nd file in this case)
      flai_usms[[usm_index]] = c(flai_usms[[usm_index]],lai_file_path[i])
    }

    # We increment the usm_index if we just treated plant 2 or if the dominance
    # of the next i is 1 (case were we only have lai file for plant 1)
    if(i < length(lai_file_path)){
      if(dominance[i] == 2 | dominance[i + 1] == 1){
        usm_index = usm_index + 1
      }
    }
  }

  if (length(usms_list) == 0){

    usms_list = full_usms_list

  } else {

    # Checking if the input usms_list is included in the full list
    usms_exist <- usms_list %in% full_usms_list

    # Error if any unknown usm name !
    if (!all(usms_exist)){
      stop("At least one usm does not exist in usms.xml file : ",usms_list[!usms_exist])
    }

  }

  # Checking XML files existence, check_files
  if (check_files) {
    all_files_exist <- check_usms_files(workspace_path = workspace_path,
                                        javastics_path = javastics_path,
                                        usms_list = usms_list)$all_exist

    if (!all(all_files_exist)) stop(paste("Missing files have been detected for usm(s):",
                                          paste(usms_list[!all_files_exist], collapse = ", ")))
  }


  # Command string without usm name
  # according to OS type
  jexe="JavaSticsCmd.exe"

  if (tolower(Sys.info()["sysname"])=="windows") {
    cmd <- jexe
    cmd_args= paste0(' --generate-txt "',workspace_path,'"')
  } else {
    cmd <- "java"
    cmd_args= paste0('-jar ',jexe,' --generate-txt "',workspace_path,'"')
  }

  usms_number <- length(usms_list)

  # Fixing dir_per_usm_flag value if FALSE and there are
  # multiple usms. In that case files will be overwritten.
  # So fixing it to TRUE
  if ( !dir_per_usm_flag && usms_number > 1 ) {
    warning("Generating files in the JavaStics workspace is not compatible with multiple usms !")
    dir_per_usm_flag <- TRUE
  }


  # For storing if all files copy were successful or not
  # for each usm
  global_copy_status <- rep(FALSE, usms_number)
  obs_copy_status = lai_copy_status = global_copy_status


  # Full list of the files to copy

  files_list <- c("climat.txt",
                  "param.sol",
                  "ficini.txt",
                  "ficplt1.txt",
                  "fictec1.txt",
                  "station.txt",
                  "new_travail.usm",
                  "tempopar.sti",
                  "tempoparv6.sti",
                  "ficplt2.txt",
                  "fictec2.txt")

  files_nb <- length(files_list)

  # Generating source files paths
  files_path <- file.path(workspace_path, files_list)

  # Fixing files linked to associated crops
  mandatory_files <- c(rep(T,9), F , F)

  # outputs definition files
  out_files_def <- c("var.mod", "rap.mod", "prof.mod")
  out_files_path <- file.path(javastics_path, "config",out_files_def)


  #start_time <- Sys.time()
  # For keeping target usms dir paths
  usms_path <- vector(mode = "character", usms_number)

  # Keeping execution status
  exec_status <- rep(TRUE, length = usms_number)

  for (i in 1:usms_number) {
    #foreach(i = 1:usms_number, .export = ".GlobalEnv") %dopar% {

    usm_name <- usms_list[i]

    # Removing all previous generated files, to be sure.
    file.remove(files_path[file.exists(files_path)])

    # Generating text files
    ret <- system2(command = cmd, args = paste(cmd_args,usm_name),
                   stdout= TRUE, stderr = TRUE)

    # Get info returned by system2 for detecting errors
    exec_status[i] <- !any(grepl(pattern = "ERROR", ret))
    if (! exec_status[i]) {
      # displaying usm name
      if(verbose) cli::cli_alert_danger("USM {.val {usm_name}} creation failed")
      next
    }

    #  dir creation for the curent usm, if needed
    if (dir_per_usm_flag) {
      usm_path <- file.path(target_path, usm_name)
      if (!dir.exists(usm_path)) dir.create(usm_path)
    } else {
      usm_path <- target_path
    }

    # TODO: evaluate if return might be done here !
    # If only one usm, for exiting the loop if target_path
    # is the workspace path, no need to copy files
    if (!dir_per_usm_flag && target_path == workspace_path) next

    # Copying files to the usm directory
    copy_status <- all(file.copy(from = files_path[file.exists(files_path)],
                                 to = usm_path, overwrite = T))

    # Copying default files for outputs definition
    out_copy_status <- all(file.copy(from = out_files_path,
                                     to = usm_path, overwrite = T))

    # Copying observation files
    obs_path <- file.path(workspace_path, paste0(usm_name,".obs"))
    if ( file.exists(obs_path) ) {
      obs_copy_status[i] <- file.copy(from = obs_path,
                                      to = usm_path, overwrite = T)
    }else{
      if(verbose) cli::cli_alert_warning("Obs file not found for USM {.val {usm_name}}: {.file {obs_path}}")
    }

    # Copying lai files if lai forcing
    if (lai_forcing[usm_name]) {
      lapply(flai_usms[usm_name], function(x){
        if(file.exists(x)){
          lai_copy_status[i] <- file.copy(from = x,
                                          to = usm_path, overwrite = T)
        }else{
          if(verbose) cli::cli_alert_warning("LAI file not found for USM {.val {usm_name}}: {.file {lai_file_path[i]}}")
        }
      })
    }
    # Storing global files copy status
    global_copy_status[i] <- copy_status & out_copy_status

    # displaying usm name
    if(verbose) cli::cli_alert_info("USM {.val {usm_name}} successfully created")

    # Storing the current usm target path
    usms_path[i] <- usm_path
  }

  # Messages if failing copies
  if (!all(global_copy_status)) {
    failed_usms <- usms_list[!global_copy_status]
    warning(paste("Errors occured while generating or copying files to usms directories for usms:\n",
                  paste(failed_usms, collapse = ", ")))
  }

  # stopping the cluster
  # stopCluster(cl)
  # duration <- Sys.time() - start_time
  # print(duration)


  # Message about execution errors
  if (!all(exec_status)) warning("Errors have been detected for usm(s):",
                                 paste(usms_list[!exec_status], collapse = ", "))

  # Returning a list of created directories and files copy status
  # for each directory ( FALSE if any files copy error )
  return(invisible(list(usms_path = usms_path, files = basename(files_path),
                        copy_status = global_copy_status,
                        obs_copy_status = obs_copy_status,
                        lai_copy_status = lai_copy_status)))


}
