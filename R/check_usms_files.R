#' Checking xml files existence got from usms.xml
#'
#' @param workspace_path The path to a JavaStics-compatible workspace
#' @param javastics_path JavaStics installation path (Optional, needed if the
#'  plant files are not in the `workspace_path`
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
#' \dontrun{
#'
#' check_usms_files(
#'   workspace_path = "/path/to/workspace",
#'   javastics_path = "/path/to/JavaSTICS/folder"
#' )
#'
#' check_usms_files(
#'   workspace_path = "/path/to/workspace",
#'   javastics_path = "/path/to/JavaSTICS/folder", usm_list = c("usm1", "usm3")
#' )
#' }
#'
#' @importFrom dplyr %>%

check_usms_files <- function(workspace_path,
                             javastics_path,
                             usms_list = NULL,
                             file_name = "usms.xml") {

  # TODO: add file type to filter: ftec, fplt,...

  # Getting usms_list
  usms_full_list <- get_usms_list(file = file.path(workspace_path, file_name))

  # Getting the full list or a subset
  if (is.null(usms_list)) {
    usms_list <- usms_full_list
  } else {
    # checking if all usms names exist
    usm_idx <- usms_full_list %in% usms_list
    if (!length(usms_list) == sum(usm_idx)) {
      usms_list <- usms_full_list[usm_idx]
      warning(paste(
        "There are missing usms in usms.xml file:\n",
        paste(usms_full_list[!usm_idx], collapse = ", ")
      ))
    }
  }

  usms_nb <- length(usms_full_list)
  par_list <- c(
    "nbplantes", "finit", "fstation", "fclim1", "fclim2", "fplt",
    "ftec", "flai"
  )

  param_usms <- get_param_xml(
    file.path(workspace_path, "usms.xml")
  )$usms.xml[par_list]
  id_plt <- sequence(param_usms$nbplantes)
  param_usms$fplt <- param_usms$fplt[param_usms$fplt != "null"]
  param_usms$ftec <- param_usms$ftec[param_usms$ftec != "null"]
  param_usms$flai <- param_usms$flai[param_usms$flai != "null"]

  usms_files <- data.frame(
    usm = usms_full_list,
    nbplantes = param_usms$nbplantes,
    finit = param_usms$finit,
    fstation = param_usms$fstation,
    fclim1 = param_usms$fclim1,
    fclim2 = param_usms$fclim2,
    fplt1 = param_usms$fplt[id_plt == 1],
    ftec1 = param_usms$ftec[id_plt == 1],
    flai1 = param_usms$flai[id_plt == 1],
    stringsAsFactors = FALSE
  )

  if (length(param_usms$fplt[id_plt == 2]) > 0) {
    usms_files$fplt2 <- param_usms$fplt[id_plt == 2]
  }
  if (length(param_usms$ftec[id_plt == 2]) > 0) {
    usms_files$ftec2 <- param_usms$ftec[id_plt == 2]
  }
  if (length(param_usms$flai[id_plt == 2]) > 0) {
    usms_files$flai2 <- param_usms$flai[id_plt == 2]
  }

  # Filtering usms
  if (!is.null(usms_list)) {
    usms_files <- usms_files %>%
      dplyr::filter(.data$usm %in% usms_list)
  }

  # fix: for intercrop used as sole crop, if nbplantes == 1, set file to "null"
  usms_files[usms_files$nbplantes == 1, c("fplt2", "ftec2", "flai2")] <- "null"




  # Checking all files existence
  all_init_exist <- file.exists(file.path(workspace_path, usms_files$finit))
  if (!all(all_init_exist)) {
    warning(paste(
      "Ini file(s)", paste(unique(usms_files$finit[!all_init_exist]),
        collapse = ","
      ),
      "not found in", workspace_path
    ))
  }

  all_sta_exist <- file.exists(file.path(workspace_path, usms_files$fstation))
  if (!all(all_sta_exist)) {
    warning(paste(
      "Station file(s)", paste(unique(usms_files$fstation[!all_sta_exist]),
        collapse = ","
      ),
      "not found in", workspace_path
    ))
  }

  fclim1_exist <- file.exists(file.path(workspace_path, c(usms_files$fclim1)))
  if (!all(fclim1_exist)) {
    warning(paste(
      "Climate file(s)", paste(unique(usms_files$fclim1[!fclim1_exist]),
        collapse = ","
      ),
      "not found in", workspace_path
    ))
  }
  fclim2_exist <- file.exists(file.path(workspace_path, c(usms_files$fclim2)))
  if (!all(fclim2_exist)) {
    warning(paste(
      "Climate file(s)", paste(unique(usms_files$fclim2[!fclim2_exist]),
        collapse = ","
      ),
      "not found in", workspace_path
    ))
  }

  all_fclim_exist <- fclim1_exist & fclim2_exist

  ftec1_exist <- file.exists(file.path(workspace_path, usms_files$ftec1))
  ftec1_exist[usms_files$ftec1 == "null"] <- TRUE
  if (!all(ftec1_exist)) {
    warning(paste(
      "Tec file(s)", paste(unique(usms_files$ftec1[!ftec1_exist]),
        collapse = ","
      ),
      "not found in", workspace_path
    ))
  }
  ftec2_exist <- file.exists(file.path(workspace_path, usms_files$ftec2))
  ftec2_exist[usms_files$ftec2 == "null"] <- TRUE
  if (!all(ftec2_exist)) {
    warning(paste(
      "Tec file(s)", paste(unique(usms_files$ftec2[!ftec2_exist]),
        collapse = ","
      ),
      "not found in", workspace_path
    ))
  }
  all_ftec_exist <- ftec1_exist & ftec2_exist

  # specific checking for fplt

  fplt1_exist <- file.exists(file.path(
    file.path(javastics_path, "plant"),
    usms_files$fplt1
  )) |
    file.exists(file.path(file.path(workspace_path, "plant"), usms_files$fplt1))
  fplt1_exist[usms_files$fplt1 == "null"] <- TRUE
  if (!all(fplt1_exist)) {
    warning(paste(
      "Plant file(s)", paste(unique(usms_files$fplt1[!fplt1_exist]),
        collapse = ","
      ),
      "not found in", workspace_path,
      ". \nPlease note that with SticsRpacks, plant folder (that contains plant files)",
      "can be located either inside the workspace or in the JavaStics path.\n",
      "Check that your plant files exist in one of these."
    ))
  }

  fplt2_exist <- file.exists(file.path(
    file.path(javastics_path, "plant"),
    usms_files$fplt2
  )) |
    file.exists(file.path(file.path(workspace_path, "plant"), usms_files$fplt2))
  fplt2_exist[usms_files$fplt2 == "null"] <- TRUE
  if (!all(fplt2_exist)) {
    warning(paste(
      "Plant file(s)", paste(unique(usms_files$fplt2[!fplt2_exist]),
        collapse = ","
      ),
      "not found in", workspace_path,
      ". Please note that with SticsRpacks, plant folder (that contains plant files)",
      "can be located either inside the workspace or in the JavaStics path.\n",
      "Check that your plant files exist in one of these."
    ))
  }




  all_fplt_exist <- fplt1_exist & fplt2_exist

  all_exist <- all_sta_exist & all_fclim_exist & all_ftec_exist &
    all_fplt_exist & all_init_exist

  # Adding files existence status
  usms_files <- dplyr::mutate(usms_files, all_exist = all_exist)

  return(invisible(usms_files))
}
