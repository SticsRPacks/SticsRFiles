#' @title Generating one or several usms directories from a javastics workspace
#' content
#'
#' @description The function creates sets of input files for one or multiple
#' usms from usms data stored in a JavaSTICS workspace. For multiple usms,
#' sets will be generated into individual folders named with usm names.
#' Observations files will be also copied if they are named `[usm_name].obs`
#' For one usm, files will be generated either in the workspace directory
#' or in a subdirectory.
#'
#' @param javastics Path of JavaSTICS. Optional (needed if the JavaSTICS
#' converter is used, java_converter set to TRUE in inputs)
#' @param workspace Path of a JavaSTICS workspace
#' (i.e. containing the STICS XML input files). Optional, if not provided
#' the current workspace stored in JavaSTICS preferences will be used.
#' @param out_dir The path of the directory where to create usms directories
#' (Optional), if not provided the JavaSTICS workspace will be used as root
#' @param usm List of usms to generate (Optional). If not provided, all
#' usms contained in workspace/usms.xml file will be generated.
#' @param stics_version the STICS files version to use (optional,
#' default to latest).
#' @param verbose Logical value TRUE (default) for displaying information
#' while running, FALSE otherwise
#' @param dir_per_usm_flag logical, optional; TRUE (default)
#' if one want to create one directory per USM, FALSE if USM files
#' are generated in the out_dir (only useful for usm of size one)
#' @param java_cmd For unix like systems, the java virtual machine command
#' name or executable path. Useful only if the JavaSTICS command line
#' is used for generating files. "java" is the default system command,
#' but a full path to a java executable (other than the default one)
#' may be given
#' @param java_converter logical, optional; TRUE for using the JavaSTICS command
#' (a JavaSTICS path must be set in the function inputs),
#' FALSE otherwise (default)
#'
#' @return A list with named elements:
#' usms_path : created directories paths (for storing STICS input files),
#' files : generated files list (in JavaSTICS workspace origin),
#' copy_status : logical value vector, indicating if all files have been
#' generated for each usm
#' obs_copy_status : logical value vector, indicating if observation files
#' have been successfully copied in usms directories
#'
#' @examples
#' \dontrun{
#' javastics <- "/path/to/JavaSTICS/folder"
#' workspace <- "/path/to/workspace"
#'
#' # For all usms
#' gen_usms_xml2txt(javastics, workspace)
#'
#' # For an usms list
#' usm_list <- c("usm1", "usm2")
#' gen_usms_xml2txt(javastics, workspace, usms = usm_list)
#' }
#'
#' @export
#'
#'

gen_usms_xml2txt <- function(
    javastics = NULL,
    workspace = NULL,
    out_dir = NULL,
    usm = NULL,
    stics_version = "latest",
    verbose = TRUE,
    dir_per_usm_flag = TRUE,
    java_cmd = "java",
    java_converter = FALSE) {
  if (java_converter) {
    # javastics directory must be given
    if (is.null(javastics)) {
      stop(
        "For using JavaSTICS commande line converter ",
        "the JavaSTICS directory must be set in function inputs !"
      )
    }

    # checking javastics path
    check_java_path(javastics)
    start_wd <- getwd()
    on.exit(setwd(start_wd))

    setwd(javastics)

    # Checking and getting JavaSTICS workspace path
    workspace <- check_java_workspace(javastics, workspace)
    if (base::is.null(workspace)) {
      return()
    }
  }

  # Setting the javastics workspace as root directory where to generate
  # usms files or directories (dir_per_usm_flag value is TRUE)
  if (base::is.null(out_dir)) out_dir <- workspace

  # Creating target dir if not exists
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  usms_file_path <- file.path(workspace, "usms.xml")
  usms_doc <- xmldocument(usms_file_path)

  # Retrieving usm names list from the usms.xml file
  full_usms_list <- get_usms_list(file = usms_file_path)

  # Do some usms have lai forcing? If so, read it accordingly:
  lai_forcing <- get_lai_forcing_xml_doc(usms_doc)

  lai_file_path <-
    file.path(
      workspace,
      get_param_value(usms_doc, param_name = "flai")$flai
    )

  dominance <- get_param_value(usms_doc, param_name = "dominance")$dominance

  nbplantes <- get_param_value(usms_doc, param_name = "nbplantes")$nbplantes

  flai_usms <- vector(mode = "list", length = length(full_usms_list))
  names(flai_usms) <- full_usms_list
  usm_index <- 1 # This is equivalent of i, but tracks which usm we are doing in
  # the for loop below, because sometimes we have two lai files

  for (i in seq_along(lai_file_path)) {
    if (dominance[i] == 1) {
      flai_usms[[usm_index]] <- lai_file_path[i]
    } else if (nbplantes[usm_index] == 2) {
      # Here we provide the lai file for the second plant, but just if
      # nbplantes = 2 because it can still be parameterized or be =null when
      # nbplantes = 1 (but we don't want the 2nd file in this case)
      flai_usms[[usm_index]] <- c(flai_usms[[usm_index]], lai_file_path[i])
    }

    # We increment the usm_index if we just treated plant 2 or if the dominance
    # of the next i is 1 (case were we only have lai file for plant 1)
    if (i < length(lai_file_path)) {
      if (dominance[i] == 2 || dominance[i + 1] == 1) {
        usm_index <- usm_index + 1
      }
    }
  }

  if (is.null(usm)) {
    usm <- full_usms_list
  } else {
    # Checking if the input usm is included in the full list
    usms_exist <- usm %in% full_usms_list

    # Error if any unknown usm name !
    if (!any(usms_exist)) {
      stop(
        "Not any usm exists in usms.xml file : ",
        usm[!usms_exist]
      )
    } else {
      if (any(!usms_exist)) {
        warning(
          "Not all usm exist in usms.xml file : ",
          paste(usm[!usms_exist], collapse = ", ")
        )
      }
    }

    # Update usm list for existing usms only in the usms.xml file
    usm <- usm[usms_exist]
  }

  # Getting files list per usm as a named list
  all_files_list <- get_files_list(
    workspace = workspace,
    javastics = javastics,
    usm = usm
  )

  # Removing non existing files from the list
  all_files_list <- lapply(all_files_list, function(x) {
    x$paths <- x$paths[x$exist]
    x$exist <- x$exist[x$exist]
    return(x)
  })

  # Checking XML files existence, check_files
  all_files_exist <- unlist(
    lapply(all_files_list, function(x) {
      all(x$exist)
    })
  )

  if (!all(all_files_exist)) {
    unknown_files <-
      unlist(
        lapply(
          all_files_list[!all_files_exist],
          function(x) {
            x$paths[!x$exist]
          }
        ),
        use.names = FALSE
      )
    miss_files_mess <- paste(
      sprintf(
        fmt = "%s: %s \n",
        usm[!all_files_exist],
        unknown_files
      ),
      collapse = ""
    )

    mess_length <- sum(nchar(miss_files_mess)) + 100L
    if (options("warning.length")$warning.length < mess_length) {
      options(warning.length = mess_length)
    }

    warning(
      "Missing files have been detected for usm(s):\n",
      miss_files_mess
    )
  }

  # Removing usms with missing files
  all_files_list <- all_files_list[all_files_exist]

  if (java_converter) {
    # Getting javastics cmd line
    cmd_list <- get_javastics_cmd(
      javastics,
      java_cmd = java_cmd,
      type = "generate",
      workspace = workspace
    )
    cmd_args <- cmd_list$cmd_generate
    cmd <- cmd_list$command
  } else {
    cmd_args <- NULL
    cmd <- NULL
  }

  usms_number <- length(usm)

  # Fixing dir_per_usm_flag value if FALSE and there are
  # multiple usms. In that case files will be overwritten.
  # So fixing it to TRUE
  if (!dir_per_usm_flag && usms_number > 1) {
    warning(
      "Generating files in the JavaSTICS workspace",
      " is not compatible with multiple usms !"
    )
    dir_per_usm_flag <- TRUE
  }

  # For storing if all files copy were successful or not
  # for each usm
  global_copy_status <- rep(FALSE, usms_number)
  obs_copy_status <- lai_copy_status <- global_copy_status

  # Full list of the text files to copy
  files_list <- c(
    "climat.txt",
    "param.sol",
    "ficini.txt",
    "ficplt1.txt",
    "fictec1.txt",
    "station.txt",
    "new_travail.usm",
    "tempopar.sti",
    "tempoparv6.sti",
    "ficplt2.txt",
    "fictec2.txt"
  )

  # Generating source files paths
  files_path <- file.path(workspace, files_list)

  # outputs definition files
  # Looking for them first in workspace, and then
  # in javastics for those that do not exist in
  # workspace
  out_files_def <- c("var.mod", "rap.mod", "prof.mod")
  out_files_java_path <- file.path(javastics, "config", out_files_def)
  out_files_work_path <- file.path(workspace, out_files_def)

  out_files_idx_path <- file.exists(out_files_work_path)
  out_files_path <- out_files_work_path[out_files_idx_path]
  if (!all(out_files_idx_path)) {
    out_files_path <- c(
      out_files_path,
      out_files_java_path[!out_files_idx_path]
    )
  }

  # For keeping target usms dir paths
  usms_path <- vector(mode = "character", usms_number)

  # Keeping execution status
  exec_status <- rep(TRUE, length = usms_number)

  for (i in 1:usms_number) {
    usm_name <- usm[i]

    # Removing all previous generated files, to be sure.
    file.remove(files_path[file.exists(files_path)])

    #  dir creation for the curent usm, if needed
    if (dir_per_usm_flag) {
      usm_path <- file.path(out_dir, usm_name)
      if (!dir.exists(usm_path)) dir.create(usm_path)
    } else {
      usm_path <- out_dir
    }

    if (java_converter) {
      # Generating text files
      ret <- system2(
        command = cmd,
        args = paste(cmd_args, usm_name),
        stdout = TRUE,
        stderr = TRUE
      )
      # Get info returned by system2 for detecting errors
      exec_status[i] <- !any(grepl(pattern = "ERROR", ret))
      if (!exec_status[i]) {
        # displaying usm name
        if (verbose) {
          cli::cli_alert_danger("USM {.val {usm_name}} creation failed")
        }
        next
      }

      # Copying generated files to the usm directory
      if (dir_per_usm_flag) {
        copy_status <- all(file.copy(
          from = files_path[file.exists(files_path)],
          to = usm_path,
          overwrite = TRUE
        ))
      }
    } else {
      usm_data <- get_usm_data(usms_doc, usm_name, workspace)

      # Getting the usm files paths
      usm_files_path <- all_files_list[[usm_name]]$paths

      # Getting climate files paths (unique paths)
      clim_files_path <- unique(
        usm_files_path[grep(
          pattern = "\\.[0-9]{4}$",
          x = usm_files_path
        )]
      )

      # Getting xml files paths
      files_idx <- grep(
        pattern = "\\.xml$",
        usm_files_path
      )
      xml_files_path <- usm_files_path[files_idx]

      # Generation status vector for xml files and new_travail.usm
      # and climate.txt
      gen_files_status <- rep(TRUE, length(xml_files_path) + 2)
      plant_id_plt <- 0
      plant_id_tec <- 0
      plant_id <- 0

      # Converting xml files to txt files
      for (f in seq_along(xml_files_path)) {
        file_path <- xml_files_path[f]

        if (grepl(pattern = "usms.xml", x = file_path)) {
          # usms.xml file is not converted
          next
        }

        found_plt <- grepl(pattern = "_plt", x = file_path)
        found_tec <- grepl(pattern = "_tec", x = file_path)

        if (found_plt) {
          plant_id_plt <- plant_id_plt + 1
          plant_id <- plant_id_plt
        }

        if (found_tec) {
          plant_id_tec <- plant_id_tec + 1
          plant_id <- plant_id_tec
        }

        # Getting soil name
        # usefull for generating sol2txt.xsl file in
        # gen_sol_xsl_file when used in convert_xml2txt
        if (grepl(pattern = "sols", x = file_path)) {
          soil_name <- usm_data$nomsol
        } else {
          soil_name <- NULL
        }

        gen_files_status[f] <- convert_xml2txt(
          file = file_path,
          stics_version = stics_version,
          out_dir = usm_path,
          plant_id = plant_id,
          soil_name = soil_name
        )
      }

      # Generating new_travail.usm
      gen_files_status[f + 1] <- gen_new_travail(
        usm_data,
        usm = usm_name,
        workspace = workspace,
        out_dir = usm_path
      )

      # Generating climat.txt file
      gen_files_status[f + 2] <- gen_climate(
        clim_files_path,
        out_dir = usm_path
      )

      # setting exec status result
      exec_status[i] <- all(gen_files_status)

      copy_status <- exec_status[i]
    }

    # Copying default files for outputs definition
    # if they do not exist in usm_path
    to_copy_idx <- !file.exists(file.path(usm_path, basename(out_files_path)))

    if (any(to_copy_idx)) {
      out_copy_status <- all(file.copy(
        from = out_files_path[to_copy_idx],
        to = usm_path,
        overwrite = TRUE
      ))
    } else {
      out_copy_status <- TRUE
    }

    # If only one usm, for exiting the loop if out_dir
    # is the workspace path, no need to copy files
    if (!dir_per_usm_flag && out_dir == workspace) {
      global_copy_status[i] <- TRUE
      next
    }

    # Copying observation files
    obs_path <- file.path(workspace, paste0(usm_name, ".obs"))
    if (file.exists(obs_path)) {
      obs_copy_status[i] <- file.copy(
        from = obs_path,
        to = usm_path,
        overwrite = TRUE
      )
    } else {
      if (verbose) {
        cli::cli_alert_warning(paste0(
          "Obs file not found for USM",
          "
                                      {.val {usm_name}}: {.file {obs_path}}"
        ))
      }
    }

    # Copying lai files (whatever the lai forcing value is)
    lapply(flai_usms[usm_name], function(x) {
      idx <- basename(x) != "null" & file.exists(x)
      x <- x[idx]
      if (length(x > 0)) {
        lai_copy_status[i] <- file.copy(
          from = x,
          to = usm_path,
          overwrite = TRUE
        )
      } else {
        if (verbose) {
          cli::cli_alert_warning(paste0(
            "LAI file not found for USM ",
            "{.val {usm_name}}: {.file ",
            "{lai_file_path[i]}}"
          ))
        }
      }
    })

    # Storing global files copy status
    global_copy_status[i] <- copy_status & out_copy_status

    # displaying usm name
    if (verbose) {
      cli::cli_alert_info("USM {.val {usm_name}} successfully created")
    }

    # Storing the current usm target path
    usms_path[i] <- usm_path
  }

  # Messages if failing copies
  if (!all(global_copy_status)) {
    failed_usms <- usm[!global_copy_status]
    warning(paste(
      "Errors occured while generating or",
      "copying files to usms directories for usms:\n",
      paste(failed_usms, collapse = ", ")
    ))
  }

  # Message about execution errors
  if (!all(exec_status)) {
    warning(
      "Errors have been detected for usm(s):",
      paste(usm[!exec_status], collapse = ", ")
    )
  }

  # Returning a list of created directories and files copy status
  # for each directory ( FALSE if any files copy error )
  return(invisible(list(
    usms_path = usms_path,
    files = basename(files_path),
    copy_status = global_copy_status,
    obs_copy_status = obs_copy_status,
    lai_copy_status = lai_copy_status
  )))
}
