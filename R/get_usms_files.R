#' Getting existing xml files path list per usm from an usms.xml file
#'
#' @param workspace_path JavaStics workspace path
#' @param usms_list Vector of usms names (Optional)
#' @param file_name Usms XML file name (Optional)
#' @param file_type Type of file to get (see Types definition)
#' @param javastics_path JavaStics installation path (Optional)
#'
#' @return A named list with existing files path in each usm element
#'
#@keywords internal
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' get_usms_files(workspace_path = "/path/to/javastics",
#' javastics_path = "path_to_javastics")
#'
#' get_usms_files(workspace_path = "/path/to/javastics",
#' javastics_path = "path_to_javastics", usm_list = c("usm1", "usm3"))
#'
#' get_usms_files(workspace_path = "/path/to/javastics",
#' file_type = "c("finit", ""ftec" ))
#'
#'
#' }
#'
get_usms_files <- function(workspace_path,
                           usms_list = NULL,
                           file_name = "usms.xml",
                           file_type = NULL,
                           javastics_path = NULL) {

  # Types definition
  files_types <- c("fplt","finit","fclim1","fclim2","fstation", "ftec")
  if (base::is.null(file_type)) {
    file_type <- files_types
  } else {
    # Checking if type(s) exist(s)
  }

  # Checking if plt files are needed
  plt_path <- NULL
  check_plt <- FALSE
  # Getting first the plant dir path in the workspace, if any
  if ("fplt" %in% file_type) {
    if(base::is.null(javastics_path)) stop("For getting plt files path, javastics_path is needed !")
    ws_plant <- file.path(workspace_path, "plant")
    if (file.exists(ws_plant)) plt_path <- ws_plant
    check_plt <- TRUE
  }

  # Getting javastics plant path
  if (check_plt) {
    plt_path <- c(plt_path, file.path(javastics_path, "plant"))
    file_type <- setdiff(file_type, "fplt")
  }

  # Getting usms.xml path
  usms_xml_path <- file.path(workspace_path, file_name)

  # Checking usms file
  if (! file.exists(usms_xml_path) ) {
    stop(paste("Unknown path:", usms_xml_path))
  }

  # Getting usms_list
  usms_full_list <- get_usms_list(usms_xml_path)[[1]]

  # Getting the full list or a subset
  if ( base::is.null(usms_list) ) {
    usms_list <- usms_full_list
  } else {
    # checking if usms names exist
    usm_idx <- usms_full_list %in% usms_list
    if (! length(usms_list) == sum(usm_idx) ) {
      usms_list <- usms_full_list[usm_idx]
      warning(paste("There are missing usms in usms.xml file:\n",
                    paste(usms_full_list[!usm_idx], collapse = ", ")))
    }
  }

  usms_nb <- length(usms_list)
  usms_files <- vector("list", usms_nb)

  # Loop over usms names
  for (i in 1:usms_nb) {
    usm_name <- usms_list[i]
    usm_files <- unlist(get_param_xml(usms_xml,
                                      file_type,
                                      parent_name = "usm",
                                      parent_sel_attr = usm_name)[[1]], use.names = F)

    usm_files <- usm_files[usm_files != "null"]
    usm_files_path <- file.path(workspace_path, usm_files)
    files_idx <- base::file.exists(usm_files_path)
    usm_files_path <- usm_files_path[files_idx]
    usm_files_all_exist <- length(usm_files) == length(usm_files_path)

    plt_files <- NULL
    plt_files_path <- NULL
    if (check_plt) {
      plt_files <- unlist(get_param_xml(usms_xml,
                                        "fplt",
                                        parent_name = "usm",
                                        parent_sel_attr = usm_name)[[1]], use.names = F)

      plt_files <- plt_files[plt_files != "null"]
      # applying for multiple paths (javastics, workspace)
      plt_files_path <- unlist(lapply(plt_path, function(x) file.path(x, plt_files)))
      plt_idx <- base::file.exists(plt_files_path)
      plt_files_path <- plt_files_path[plt_idx]
      plt_files_all_exist <- length(plt_files) == length(plt_files_path)
    }
    usms_files[[i]] <- list(paths=c(usm_files_path, plt_files_path),
                            all_exist=usm_files_all_exist & plt_files_all_exist)
  }

  # Returning a named list
  names(usms_files) <- usms_list
  return(usms_files)
}
