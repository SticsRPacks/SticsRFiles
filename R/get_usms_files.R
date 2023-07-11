#' Getting existing xml files path list per usm from an usms.xml file
#'
#' @param workspace Path of a JavaSTICS workspace (i.e. containing the STICS
#' XML input files)
#' @param usms_list Vector of usms names (Optional)
#' @param usms_file Path (including name) of a USM XML file.
#' @param file_type Vector of file(s) type to get (if not given,
#' all types are returned, see details)
#' @param javastics Path of JavaSTICS Optional, only needed if the plant files
#' are not in the workspace
#' (in this case the plant files used are those included in the
#' JavaSTICS distribution)
#' @param workspace_path `r lifecycle::badge("deprecated")` `workspace_path`
#' is no longer supported, use `workspace` instead.
#' @param file_name `r lifecycle::badge("deprecated")` `file_name` is no
#'   longer supported, use `usms_file` instead.
#' @param javastics_path `r lifecycle::badge("deprecated")` `javastics_path`
#' is no longer supported, use `javastics` instead.
#'
#' @details The possible values of file types are: "fplt", "finit", "fclim1",
#' "fclim2", "fstation", "ftec", "sols", "pargen" and "parnew"
#'
#' @return A named list with existing files path in each usm element
#'
#' @export
#' @seealso See `get_soils_list()` to get all soils in a usm file,
#' and `get_usms_list()` to get the list of usms.
#'
#' @examples
#' \dontrun{
#'
#' get_usms_files(
#'   workspace = "/path/to/workspace",
#'   javastics = "/path/to/JavaSTICS/folder"
#' )
#'
#' get_usms_files(
#'   workspace = "/path/to/workspace",
#'   javastics = "/path/to/JavaSTICS/folder", usm_list = c("usm1", "usm3")
#' )
#'
#' get_usms_files(
#'   workspace = "/path/to/workspace",
#'   file_type = c("finit", "ftec")
#' )
#' }
#'
get_usms_files <- function(workspace,
                           usms_list = NULL,
                           usms_file = "usms.xml",
                           file_type = NULL,
                           javastics = NULL,
                           workspace_path = lifecycle::deprecated(),
                           file_name = lifecycle::deprecated(),
                           javastics_path = lifecycle::deprecated()) {
  if (lifecycle::is_present(workspace_path)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_usms_files(workspace_path)",
                              "get_usms_files(workspace)")
  } else {
    workspace_path <- workspace # to remove when we update inside the function
  }
  if (lifecycle::is_present(file_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_usms_files(file_name)",
                              "get_usms_files(usms_file)")
  } else {
    file_name <- usms_file # to remove when we update inside the function
  }
  if (lifecycle::is_present(javastics_path)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_usms_files(javastics_path)",
                              "get_usms_files(javastics)")
  } else {
    javastics_path <- javastics # to remove when we update inside the function
  }

  # Types definition
  files_types <- c("fplt", "finit", "fclim1", "fclim2", "fstation", "ftec",
                   "sols", "pargen", "parnew")
  if (is.null(file_type)) {
    file_type <- files_types
  } else {
    # Checking if type(s) exist(s)
  }

  # Checking if plt files are needed
  plt_dir_path <- NULL
  check_plt <- FALSE

  # Getting first the plant dir path from the workspace, if any
  if ("fplt" %in% file_type) {
    javastics_plt_path <- NULL
    ws_plt_path <- NULL

    if (!base::is.null(javastics_path) &&
        dir.exists(file.path(javastics_path, "plant"))) {
      javastics_plt_path <- suppressWarnings(
        normalizePath(file.path(javastics_path, "plant"))
      )
    }

    if (dir.exists(file.path(workspace_path, "plant"))) {
      ws_plt_path <- suppressWarnings(
        normalizePath(file.path(workspace_path, "plant"))
      )
    }

    plt_dir_path <- c(ws_plt_path, javastics_plt_path)

    if (base::is.null(plt_dir_path)) {
      stop("not any plant folder found, please add javastics_path directory",
           "as function input argument or a workspace plant directory !")
    }
    check_plt <- TRUE
    file_type <- setdiff(file_type, "fplt")
  }

  # Getting usms.xml path
  usms_xml_path <- file.path(workspace_path, file_name)

  # Checking usms file
  if (!file.exists(usms_xml_path)) {
    stop(paste("Unknown path:", usms_xml_path))
  }

  # Getting usms_list
  usms_full_list <- get_usms_list(file = usms_xml_path)

  # Getting the full list or a subset
  if (is.null(usms_list)) {
    usms_list <- usms_full_list
  } else {
    # checking if usms names exist
    usm_idx <- usms_full_list %in% usms_list
    if (!length(usms_list) == sum(usm_idx)) {
      usms_list <- usms_full_list[usm_idx]
      warning(paste(
        "There are missing usms in usms.xml file:\n",
        paste(usms_full_list[!usm_idx], collapse = ", ")
      ))
    }
  }

  usms_nb <- length(usms_list)
  usms_files_list <- vector("list", usms_nb)

  # Loop over usms names
  for (i in 1:usms_nb) {
    usm_name <- usms_list[i]
    usm_files <- unlist(get_param_xml(
      file = usms_xml_path,
      param = file_type,
      select = "usm",
      select_value = usm_name,
      to_num = FALSE
    ), use.names = FALSE)

    # For selecting plant files regarding plants number
    plants_sel <- 1:get_plants_nb(usms_file = usms_xml_path,
                                  usms_list = usm_name)

    # Getting all usms xml files, except plant files
    usm_files <- unique(usm_files[usm_files != "null"])
    usm_files_path <- file.path(workspace_path, usm_files)


    # filtering tec files selection
    tec_files <- unlist(get_param_xml(
      file = usms_xml_path,
      param = "ftec",
      select = "usm",
      select_value = usm_name,
      to_num = FALSE
    )[[1]], use.names = FALSE)

    tec_files_to_rm <- tec_files[tec_files != "null"][setdiff(1:2, plants_sel)]
    # removing useless tec files
    usm_files <- setdiff(usm_files, tec_files_to_rm)
    usm_files_path <- setdiff(
      usm_files_path,
      file.path(workspace_path, tec_files_to_rm)
    )

    # Checking if all files exist
    #files_idx <- file.exists(usm_files_path)
    #usm_files_path <- usm_files_path[files_idx]
    #usm_files_all_exist <- length(usm_files) == length(usm_files_path)

    usm_files_all_exist <- file.exists(usm_files_path)


    # Specific tec files management
    plt_files <- NULL
    plt_files_path <- NULL

    if (check_plt) {
      plt_files <- unlist(get_param_xml(
        file = usms_xml_path,
        param = "fplt",
        select = "usm",
        select_value = usm_name,
        to_num = FALSE
      )[[1]], use.names = FALSE)

      # getting plant files names
      plt_files <- plt_files[plt_files != "null"][plants_sel]

      # getting plant files path in workspace or JavaSTICS/config folders
      plt_files_path <- vector(mode = "character", length(plt_files))
      plt_files_exist <- vector(mode = 'logical', length(plt_files))
      for (p in seq_along(plt_files)) {
        plt_path <- file.path(plt_dir_path[1], plt_files[p])
        plt_exist <- file.exists(plt_path)

        if (!plt_exist && length(plt_dir_path) > 1) {
          plt_path <- file.path(plt_dir_path[2], plt_files[p])
          plt_exist <- file.exists(plt_path)
        }
        plt_files_path[p] <- plt_path
        plt_files_exist[p] <- plt_exist
      }
    }

    # soil file
    sols_file_path <- NULL
    sols_file_exists <- TRUE
    if ("sols" %in% file_type) {
      sols_file_path <- suppressWarnings(
        normalizePath(file.path(workspace_path, "sols.xml"))
      )
      sols_file_exists <- file.exists(sols_file_path)
    }

    # param_gen.xml
    pargen_file_path <- NULL
    pargen_file_exists <- TRUE
    if ("pargen" %in% file_type) {
      pargen_file_path <- suppressWarnings(
        normalizePath(file.path(workspace_path, "param_gen.xml"))
      )

      if (!file.exists(pargen_file_path))
        pargen_file_path <- suppressWarnings(
          normalizePath(file.path(javastics, "config", "param_gen.xml"))
        )

      pargen_file_exists <- file.exists(pargen_file_path)
    }

    # param_newform.xml
    parnew_file_path <- NULL
    parnew_file_exists <- TRUE
    if ("parnew" %in% file_type) {
      parnew_file_path <- suppressWarnings(
        normalizePath(file.path(workspace_path, "param_newform.xml"))
      )

      if (!file.exists(parnew_file_path))
        parnew_file_path <- suppressWarnings(
          normalizePath(file.path(javastics, "config", "param_newform.xml"))
        )

      parnew_file_exists <- file.exists(parnew_file_path)
    }

    #
    # Adding the files lists
    usms_files_list[[i]] <- list(
      paths = c(usm_files_path, plt_files_path, sols_file_path,
                pargen_file_path, parnew_file_path),
      all_exist = c(usm_files_all_exist,
                    plt_files_exist,
                    sols_file_exists,
                    pargen_file_exists,
                    parnew_file_exists)
    )
  }

  # Returning a named list
  names(usms_files_list) <- usms_list
  return(usms_files_list)
}
