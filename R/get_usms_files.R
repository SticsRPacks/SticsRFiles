#' Getting existing xml files path list per usm from an usms.xml file
#'
#' @param workspace Path of a JavaSTICS workspace (i.e. containing the STICS
#' XML input files)
#' @param usm Vector of usms names (Optional)
#' @param usms_file Path (including name) of a USM XML file.
#' @param file_type Vector of file(s) type to get (if not given,
#' all types are returned, see details)
#' @param javastics Path of JavaSTICS (optional), only needed when the plant
#' files, general parameter files or .mod files are not in the workspace
#' but included in the JavaSTICS distribution
#' @param use_mod_files logical if TRUE adding the .mod files to the list
#'
#' @details The possible values for file_type are: "fplt", "finit", "fclim1",
#' "fclim2", "fstation", "ftec", "sols", "pargen", "parnew", "usms", "mod"
#'
#' @return A named list (usms names) with files path in `paths` field and
#' a logical vector in `exist` field indicating if the file exists or not.
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
#'   usm = c("usm1", "usm3")
#' )
#'
#' get_usms_files(
#'   workspace = "/path/to/workspace",
#'   file_type = c("finit", "ftec")
#' )
#'
#' get_usms_files(
#'   workspace = "/path/to/workspace",
#'   use_mod_files = TRUE,
#'   javastics = "/path/to/JavaSTICS/folder"
#' )
#' }
#'
get_usms_files <- function(
  workspace,
  usm = NULL,
  usms_file = "usms.xml",
  file_type = NULL,
  javastics = NULL,
  use_mod_files = FALSE
) {
  # Types definition
  files_types <- c(
    "fplt",
    "finit",
    "fclim1",
    "fclim2",
    "fstation",
    "ftec",
    "flai",
    "fobs",
    "sols",
    "pargen",
    "parnew",
    "usms",
    "mod"
  )

  # files names definition
  files_names <- c(
    "fplt" = "[name]_plt.xml",
    "finit" = "[name]_ini.xml",
    "fclim1" = "[name].YYYY",
    "fclim2" = "[name].YYYY",
    "fstation" = "[name]_sta.xml",
    "ftec" = "[name]_tec.xml",
    "flai" = "[name].lai.",
    "fobs" = "[name].obs",
    "sols" = "sols.xml",
    "pargen" = "param_gen.xml",
    "parnew" = "param_newform.xml",
    "usms" = "usms.xml",
    "mod" = c("var.mod", "rap.mod", "prof.mod")
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
  usms_full_list <- get_usms_list(usms_xml_path)

  # Getting the full list or a subset
  if (is.null(usm)) {
    usms_list <- usms_full_list
  } else {
    # checking if usms names exist
    # not any match
    usm_idx <- usms_full_list %in% usm
    if (length(usm_idx) == 0) {
      stop("No usm found in the usms.xml file")
    }
    # some usms not found
    if (!length(usm) == sum(usm_idx)) {
      warning(paste(
        "There are missing usms in usms.xml file:\n",
        paste(usms_full_list[!usm_idx], collapse = ", ")
      ))
    } else {
      # getting the usms wanted
      usms_list <- usms_full_list[usm_idx]
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
    # useless_files_idx <- grep("\\.obs|\\.lai|null", usm_files)
    useless_files_idx <- grep("null", usm_files)

    if (length(useless_files_idx) > 0) {
      usm_files <- usm_files[-useless_files_idx]
    }

    # adding usms.xml file if asked
    if ("usms" %in% file_type) {
      usm_files <- c(usms_file, usm_files)
    }

    # getting files path in workspace
    usm_files_path <- file.path(workspace, usm_files)

    # Specific plt files management, so filtering them out
    plt_idx <- grep("_plt\\.xml$", usm_files_path)
    if (length(plt_idx) > 0) {
      plt_files <- basename(usm_files_path[plt_idx])
      usm_files_path <- usm_files_path[-plt_idx]
    }

    usm_files_exist <- file.exists(usm_files_path)

    plt_files_exist <- vector("logical", 0)
    plt_files_path <- NULL

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

    # add mod files if asked
    mod_file_exists <- vector("logical", 0)
    if ("mod" %in% file_type && use_mod_files) {
      # && use_mod_files) {
      # getting mod files list from the workspace
      mod_files_path <- list.files(
        path = workspace,
        pattern = "\\.mod$",
        full.names = TRUE
      )

      # If no mod files found and javastics not given
      if (length(mod_files_path) == 0) {
        if (is.null(javastics)) {
          warning(
            "No .mod files found in the workspace,",
            " please add javastics directory as function input argument !"
          )
        } else {
          # getting mod files list from the javastics folder
          javastics_mod_files_path <- list.files(
            path = file.path(javastics, "config"),
            pattern = "\\.mod$",
            full.names = TRUE
          )
        }
      }
      # diff list of mod files with those existing in javastics
      # completion of mod files with javastics ones if not in workspace
      if (exists("javastics_mod_files_path")) {
        mod_diff <- setdiff(
          basename(javastics_mod_files_path),
          basename(mod_files_path)
        )
        if (length(mod_diff)) {
          mod_files_path <- c(
            mod_files_path,
            file.path(javastics, "config", mod_diff)
          )
        }
      }
      mod_file_exists <- file.exists(mod_files_path)
    }
    # Adding the files lists
    usms_files_list[[i]] <- list(
      paths = c(
        usm_files_path,
        plt_files_path,
        sols_file_path,
        pargen_file_path,
        parnew_file_path
      ),
      exist = c(
        usm_files_exist,
        plt_files_exist,
        sols_file_exists,
        pargen_file_exists,
        parnew_file_exists
      )
    )
    if (use_mod_files && exists("mod_files_path")) {
      usms_files_list[[i]][["paths"]] <- c(
        usms_files_list[[i]][["paths"]],
        mod_files_path
      )
      usms_files_list[[i]][["exist"]] <- c(
        usms_files_list[[i]][["exist"]],
        mod_file_exists
      )
    }
  }

  # Returning a named list
  names(usms_files_list) <- usms_list
  usms_files_list
}
