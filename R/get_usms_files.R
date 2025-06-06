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
#' @param df_output logical if TRUE returning a data.frame, otherwise returning
#' a named list if FALSE (default)
#'
#' @details The possible values for file_type are: "fplt", "finit", "fclim1",
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
#'   javastics = "/path/to/JavaSTICS/folder",
#'   usm_list = c("usm1", "usm3")
#' )
#'
#' get_usms_files(
#'   workspace = "/path/to/workspace",
#'   file_type = c("finit", "ftec")
#' )
#' }
#'
get_usms_files <- function(
  workspace,
  usms_list = NULL,
  usms_file = "usms.xml",
  file_type = NULL,
  javastics = NULL,
  df_output = FALSE
) {
  # Types definition
  files_types <- c(
    "fplt",
    "finit",
    "fclim1",
    "fclim2",
    "fstation",
    "ftec",
    "sols",
    "pargen",
    "parnew"
  )
  if (is.null(file_type)) {
    file_type <- files_types
  } else {
    # Checking if type(s) exist(s)
    type_exists <- file_type %in% files_types
    if (!all(type_exists)) {
      warning(
        "One or more file type(s) do(es) not exist : ",
        paste(file_type[!type_exists], collapse = ",")
      )
    }
    file_type <- file_type[type_exists]
  }

  # Checking if plt files are needed
  plt_dir_path <- NULL
  check_plt <- FALSE

  # Getting first the plant dir path from the workspace, if any
  if ("fplt" %in% file_type) {
    javastics_plt_path <- NULL
    ws_plt_path <- NULL

    if (
      !base::is.null(javastics) &&
        dir.exists(file.path(javastics, "plant"))
    ) {
      javastics_plt_path <- suppressWarnings(
        normalizePath(file.path(javastics, "plant"))
      )
    }

    if (dir.exists(file.path(workspace, "plant"))) {
      ws_plt_path <- suppressWarnings(
        normalizePath(file.path(workspace, "plant"))
      )
    }

    plt_dir_path <- c(ws_plt_path, javastics_plt_path)

    if (base::is.null(plt_dir_path)) {
      stop(
        "not any plant folder found, please add javastics directory",
        " as function input argument or a workspace plant sub-directory !"
      )
    }
    check_plt <- TRUE
  }

  # Getting usms.xml path
  usms_xml_path <- file.path(workspace, usms_file)

  # Checking usms file
  if (!file.exists(usms_xml_path)) {
    stop(paste("Unknown path:", usms_xml_path))
  }

  xml_doc <- xmldocument(usms_xml_path)

  # Getting usms_list
  usms_full_list <- find_usms_soils_names(
    xml_doc = xml_doc,
    xml_name = "usm"
  )

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

    nodeset <- XML::getNodeSet(
      doc = xml_doc@content,
      path = paste0(
        "//usm[@nom='",
        usm_name,
        "']//*[starts-with(name(), 'f')]"
      )
    )

    node_names <- unlist(lapply(nodeset, XML::xmlName))
    usm_files <- XML::xmlValue(nodeset)
    usm_files <- usm_files[node_names %in% file_type]

    # Keeping usms xml files, except plant files, obs, lai, null
    useless_files_idx <- grep("\\.obs|\\.lai|null", usm_files)

    if (length(useless_files_idx) > 0) {
      usm_files <- usm_files[-useless_files_idx]
    }

    usm_files_path <- file.path(workspace, usm_files)

    # Specific plt files management
    plt_idx <- grep("_plt\\.xml$", usm_files_path)
    plt_files <- basename(usm_files_path[plt_idx])
    plt_files_path <- NULL
    usm_files_path <- usm_files_path[-plt_idx]

    usm_files_exist <- file.exists(usm_files_path)

    plt_files_exist <- vector("logical", 0)

    if (check_plt) {
      # getting plant files path in workspace or JavaSTICS/config folders
      plt_files_path <- vector(mode = "character", length(plt_files))
      plt_files_exist <- vector(mode = "logical", length(plt_files))

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
    sols_file_exists <- vector("logical", 0)
    if ("sols" %in% file_type) {
      sols_file_path <- suppressWarnings(
        normalizePath(file.path(workspace, "sols.xml"))
      )
      sols_file_exists <- file.exists(sols_file_path)
    }

    # param_gen.xml
    pargen_file_path <- NULL
    pargen_file_exists <- vector("logical", 0)
    if ("pargen" %in% file_type) {
      pargen_file_path <- suppressWarnings(
        normalizePath(file.path(workspace, "param_gen.xml"))
      )

      if (!file.exists(pargen_file_path)) {
        pargen_file_path <- suppressWarnings(
          normalizePath(file.path(javastics, "config", "param_gen.xml"))
        )
      }

      pargen_file_exists <- file.exists(pargen_file_path)
    }

    # param_newform.xml
    parnew_file_path <- NULL
    parnew_file_exists <- vector("logical", 0)
    if ("parnew" %in% file_type) {
      parnew_file_path <- suppressWarnings(
        normalizePath(file.path(workspace, "param_newform.xml"))
      )

      if (!file.exists(parnew_file_path)) {
        parnew_file_path <- suppressWarnings(
          normalizePath(file.path(javastics, "config", "param_newform.xml"))
        )
      }

      parnew_file_exists <- file.exists(parnew_file_path)
    }

    #
    # Adding the files lists
    usms_files_list[[i]] <- list(
      paths = c(
        usm_files_path,
        plt_files_path,
        sols_file_path,
        pargen_file_path,
        parnew_file_path
      ),
      all_exist = c(
        usm_files_exist,
        plt_files_exist,
        sols_file_exists,
        pargen_file_exists,
        parnew_file_exists
      )
    )
  }

  # Returning a named list
  names(usms_files_list) <- usms_list

  # returning a data.frame or a list (default)
  if (df_output) {
    return(dplyr::bind_rows(usms_files_list, .id = "usm"))
  } else {
    return(usms_files_list)
  }
}
