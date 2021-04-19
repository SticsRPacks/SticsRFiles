#' Checking xml files existence got from usms.xml
#'
#' @param workspace_path The path to a JavaStics-compatible workspace
#' @param javastics_path JavaStics installation path (Optional, needed if the plant files are not in the `workspace_path`
#' but rather in the JavaStics default plant folder)
#' @param usms_list Vector of usms names (Optional)
#' @param file_name Usms XML file name (Optional)
#'
#' @return A data.frame with files names usm row and a status (all_exist)
#' indicating weither or not all listed files exist in the `workspace_path`
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#'
#' check_usms_files(workspace_path = "/path/to/workspace",
#' javastics_path = "/path/to/javastics")
#'
#' check_usms_files(workspace_path = "/path/to/workspace",
#' javastics_path = "/path/to/javastics", usm_list = c("usm1", "usm3"))
#'
#' }
#'
#' @importFrom dplyr %>%

check_usms_files <- function(workspace_path,
                             javastics_path,
                             usms_list = NULL,
                             file_name = "usms.xml") {

  # TODO: add file type to filter: ftec, fplt,...

  # Getting usms_list
  usms_full_list <- get_usms_list(usm_path = file.path(workspace_path, file_name))

  # Getting the full list or a subset
  if(is.null(usms_list)){
    usms_list <- usms_full_list
  }else{
    # checking if all usms names exist
    usm_idx <- usms_full_list %in% usms_list
    if(!length(usms_list) == sum(usm_idx)){
      usms_list <- usms_full_list[usm_idx]
      warning(paste("There are missing usms in usms.xml file:\n",
                    paste(usms_full_list[!usm_idx], collapse = ", ")))
    }
  }

  usms_nb <- length(usms_full_list)
  par_list <- c("nbplantes", "finit", "fstation", "fclim1", "fclim2", "fplt", "ftec", "flai")

  param_usms <- get_param_xml(file.path(workspace_path,"usms.xml"))$usms.xml[par_list]

  id_plt_1 <- seq(1,to = usms_nb, by = 2)
  id_plt_2 <- seq(2,to = usms_nb, by = 2)

  usms_files <- data.frame(usm = usms_full_list,
                           nbplantes = param_usms$nbplantes,
                           finit = param_usms$finit,
                           fstation = param_usms$fstation,
                           fclim1 = param_usms$fclim1,
                           fclim2 = param_usms$fclim2,
                           fplt1 = param_usms$fplt[id_plt_1],
                           fplt2 = param_usms$fplt[id_plt_2],
                           ftec1 = param_usms$ftec[id_plt_1],
                           ftec2 = param_usms$ftec[id_plt_2],
                           flai1 = param_usms$flai[id_plt_1],
                           flai2 = param_usms$flai[id_plt_2],
                           stringsAsFactors = FALSE)

  # Filtering usms
  if (!is.null(usms_list)) {
    usms_files <- usms_files %>%
      dplyr::filter(.data$usm %in% usms_list)
  }

  # fix: for intercrop used as sole crop, if nbplantes == 1, set file to "null"
  usms_files[usms_files$nbplantes == 1,"fplt2"] <- "null"




  # Checking all files existence
  all_init_exist <- file.exists(file.path(workspace_path, usms_files$finit))

  all_sta_exist <- file.exists(file.path(workspace_path, usms_files$fstation))

  fclim1_exist <- file.exists(file.path(workspace_path, c(usms_files$fclim1)))
  fclim2_exist <- file.exists(file.path(workspace_path, c(usms_files$fclim2)))

  all_fclim_exist <- fclim1_exist & fclim2_exist

  ftec1_exist <- file.exists(file.path(workspace_path, usms_files$ftec1))
  ftec1_exist[usms_files$ftec1=="null"] <- TRUE
  ftec2_exist <- file.exists(file.path(workspace_path, usms_files$ftec2))
  ftec2_exist[usms_files$ftec2=="null"] <- TRUE
  all_ftec_exist <- ftec1_exist & ftec2_exist

  # specific checking for fplt

  fplt1_exist <- file.exists(file.path(file.path(javastics_path, "plant"), usms_files$fplt1)) |
    file.exists(file.path(file.path(workspace_path, "plant"), usms_files$fplt1))
  fplt1_exist[usms_files$fplt1=="null"] <- TRUE

  fplt2_exist <- file.exists(file.path(file.path(javastics_path, "plant"), usms_files$fplt2)) |
    file.exists(file.path(file.path(workspace_path, "plant"), usms_files$fplt2))
  fplt2_exist[usms_files$fplt2=="null"] <- TRUE

  all_fplt_exist <- fplt1_exist & fplt2_exist

  all_exist <- all_sta_exist & all_fclim_exist & all_ftec_exist & all_fplt_exist & all_init_exist

  # Adding files existence status
  usms_files <- dplyr::mutate(usms_files, all_exist = all_exist)

  return(invisible(usms_files))
}
