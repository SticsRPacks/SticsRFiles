#' Upgrade _tec.xml  file(s) to a next major version
#'
#' @param file xml technical file path or a vector of
#' @param out_dir Output directory path
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the initial version
#' @param param_newform_file Path of the param_newform.xml file corresponding
#' to the file version
#' @param ... additional argument(s) to pass to called functions
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
#' @examples
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_tec_xml(
#'   file = file.path(dir_path, "file_tec.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml"),
#'   param_newform_file = file.path(dir_path, "param_newform.xml")
#' )
#'
upgrade_tec_xml <- function(
  file,
  out_dir,
  param_gen_file,
  param_newform_file,
  ...,
  overwrite = FALSE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = param_gen_file)

  # switch over versions
  if (file_version == 9) {
    upgrade_tec_xml_9_10(
      file,
      out_dir,
      param_gen_file,
      param_newform_file,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_tec_xml_10_11(
      file = file,
      out_dir = out_dir,
      ...,
      overwrite = overwrite
    )
  }
}

#' Upgrade _plt.xml file(s) to a next major version
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the initial version
#' @param param_newform_file Path of the param_newform.xml file corresponding
#' to the file version
#' @param ... additional argument(s) to pass to called functions
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#' @param warning Logical for rising warnings, FALSE otherwise
#'
#'
#' @return None
#'
#' @export
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_plt_xml(
#'   file = file.path(dir_path, "file_plt.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml"),
#'   param_newform_file = file.path(dir_path, "param_newform.xml")
#' )
#'
upgrade_plt_xml <- function(
  file,
  out_dir,
  param_gen_file,
  param_newform_file,
  ...,
  overwrite = FALSE,
  warning = TRUE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = param_gen_file)

  # switch over versions
  if (file_version == 9) {
    upgrade_plt_xml_9_10(
      file,
      out_dir,
      param_gen_file,
      param_newform_file,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_plt_xml_10_11(
      file = file,
      out_dir = out_dir,
      ...,
      overwrite = overwrite,
      warning = warning
    )
  }
}


#' Upgrade _sta.xml file(s) to a next major version
#'
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the initial version
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_sta_xml(
#'   file = file.path(dir_path, "file_sta.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_sta_xml <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = param_gen_file)

  # switch over versions
  if (file_version == 9) {
    upgrade_sta_xml_9_10(
      file,
      out_dir,
      param_gen_file,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_sta_xml_10_11(
      file = file,
      out_dir = out_dir,
      overwrite = overwrite
    )
  }
}


#' Upgrade _ini.xml file(s) to a next major version
#'
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the initial version
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_ini_xml(
#'   file = file.path(dir_path, "file_ini.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_ini_xml <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = param_gen_file)

  # switch over versions
  if (file_version == 9) {
    upgrade_ini_xml_9_10(
      file,
      out_dir,
      param_gen_file,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_ini_xml_10_11(
      file = file,
      out_dir = out_dir,
      overwrite = overwrite
    )
  }
}

#' Upgrade a param_gen.xml file to a next major version
#'
#' @param file xml param_gen file path or a vector of
#' @param out_dir Output directory path
# @param hauteur_threshold new parameter for V11
# @param par_to_net new parameter for V11
#' @param ... additional argument(s) to pass to called functions
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
#' @examples
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_param_gen_xml(
#'   file = file.path(dir_path, "param_gen.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_param_gen_xml <- function(
  file,
  out_dir,
  # hauteur_threshold = NULL,
  # par_to_net = NULL,
  ...,
  overwrite = FALSE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = file)

  # switch over versions
  if (file_version == 9) {
    upgrade_param_gen_xml_9_10(
      file,
      out_dir,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_param_gen_xml_10_11(
      file = file,
      out_dir = out_dir,
      ...,
      overwrite = overwrite
    )
  }
}

#' Upgrade a param_newform.xml file to a next major version
#'
#' @param file xml param_newform file path or a vector of
#' @param out_dir Output directory path
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param ... additional argument(s) to pass to called functions
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
# @param use_patho Logical TRUE if code_patho is to be added to the
# param_newform.xml file, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_param_newform_xml(
#'   file = file.path(dir_path, "param_newform.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_param_newform_xml <- function(
  file,
  out_dir,
  param_gen_file,
  ...,
  overwrite = FALSE #,
  #use_patho = FALSE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = param_gen_file)

  # switch over versions
  if (file_version == 9) {
    upgrade_param_newform_xml_9_10(
      file,
      out_dir,
      param_gen_file,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_param_newform_xml_10_11(
      file = file,
      out_dir = out_dir,
      ...,
      overwrite = overwrite
    )
  }
}


#' Upgrade a sols.xml file to a next major version
#'
#' @param file xml sols file path
#' @param out_dir Output directory path
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_sols_xml(
#'   file = file.path(dir_path, "sols.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_sols_xml <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = param_gen_file)

  # switch over versions
  if (file_version == 9) {
    upgrade_sols_xml_9_10(
      file,
      out_dir,
      param_gen_file,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_sols_xml_10_11(
      file = file,
      out_dir = out_dir,
      overwrite = overwrite
    )
  }
}

#' Upgrade a usms.xml file to a next major version
#'
#' @param file xml usms file path
#' @param out_dir Output directory path
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_usms_xml(
#'   file = file.path(dir_path, "usms.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_usms_xml <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  # Getting file version if upgradable
  file_version <- file_upgradable_version(file, param_gen_file = param_gen_file)

  # switch over versions
  if (file_version == 9) {
    upgrade_usms_xml_9_10(
      file,
      out_dir,
      param_gen_file,
      overwrite = overwrite
    )
  }

  if (file_version == 10) {
    upgrade_usms_xml_10_11(
      file = file,
      out_dir = out_dir,
      overwrite = overwrite
    )
  }
}


#' Upgrade a Javastics STICS xml workspace directory to a next major version
#'
#' @param workspace JavaStics xml workspace path
#' @param out_dir   Output directory path
#' @param ... additional argument(s) to pass to called functions
#' @param javastics Path of JavaSTICS containing the STICS version corresponding
#' to the version of the files to be converted (optional)
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#' @param verbose   logical, TRUE for displaying a copy message
#' FALSE otherwise (default)
# @param use_patho Logical TRUE if code_patho is to be added to the
# param_newform.xml file, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#' upgrade_workspace_xml(
#'   workspace = "/path/to/JavaSTICS/workspace",
#'   out_dir = "/path/to/an/output/directory",
#'   javastics = "/path/to/JavaSTICS/folder"
#' )
#' }
#'
upgrade_workspace_xml <- function(
  workspace,
  out_dir,
  ...,
  javastics = NULL,
  overwrite = FALSE,
  verbose = FALSE #,
  #use_patho = FALSE
) {
  # Just in case, creating the target directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Ajout gestion des args ...
  # comment dispatcher en fonctions des fichiers et versions ???
  dots_args <- list(...)
  dots_args_names <- names(dots_args)

  # # Getting param_gen.xml path
  par_gen <- get_param_gen_file(
    file = "param_gen.xml",
    workspace = workspace,
    javastics = javastics
  )
  #
  stics_version <- get_major_version(get_xml_file_version(par_gen))

  if (verbose) {
    message(
      paste(
        "Upgrading files from version",
        stics_version,
        "to",
        stics_version + 1,
        "\n"
      ),
      paste("From: ", workspace, "\n"),
      paste("To: ", out_dir, "\n"),
      "-----------------------------------\n"
    )
  }
  #
  upgrade_param_gen_xml(
    file = par_gen,
    out_dir = out_dir,
    ...,
    overwrite = overwrite
  )
  #
  if (verbose) {
    message("param_gen.xml\n")
  }
  #
  # # Getting param_newform.xml path
  par_new <- get_param_gen_file(
    file = "param_newform.xml",
    workspace = workspace,
    javastics = javastics
  )
  #
  upgrade_param_newform_xml(
    file = par_new,
    out_dir = out_dir,
    param_gen_file = par_gen,
    ...,
    overwrite = overwrite #,
    #use_patho = use_patho
  )
  #
  if (verbose) {
    message("param_new_form.xml\n")
  }
  #
  # # Converting usms.xml file
  usms <- file.path(workspace, "usms.xml")
  upgrade_usms_xml(
    usms,
    out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )
  #
  if (verbose) {
    message("usms.xml\n")
  }
  #
  # # Converting sols.xml file
  sols <- file.path(workspace, "sols.xml")
  upgrade_sols_xml(
    sols,
    out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )
  #
  if (verbose) {
    message("sols.xml\n")
  }
  #
  # # Converting station files (*_sta.xml)
  sta_files <- get_in_files(in_dir_or_files = workspace, kind = "sta")
  upgrade_sta_xml(
    sta_files,
    out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )
  #
  if (verbose) {
    message("*_sta.xml\n")
  }
  #
  # # Converting initialisation files (*_ini.xml)
  ini_files <- get_in_files(in_dir_or_files = workspace, kind = "ini")
  upgrade_ini_xml(
    ini_files,
    out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )
  #
  if (verbose) {
    message("*_ini.xml\n")
  }

  # Converting crop management files (*_tec.xml)
  tec_files <- get_in_files(in_dir_or_files = workspace, kind = "tec")
  # code_strip = NULL
  # nrow = NULL
  # if ("code_strip" %in% dots_args_names) code_strip <- dots_args$code_strip
  # if ("nrow" %in% dots_args_names) nrow <- dots_args$nrow
  upgrade_tec_xml(
    tec_files,
    out_dir,
    param_gen_file = par_gen,
    param_newform_file = par_new,
    #code_strip = code_strip,
    #nrow = nrow,
    ...,
    overwrite = overwrite
  )

  if (verbose) {
    message("*_tec.xml\n")
  }

  # Upgrading plant files
  # if a plant sub directory exists in workspace
  usms_plt_files <-
    unique(unlist(get_param_xml(file = usms, param = "fplt")$usms$fplt))
  usms_plt_files <- usms_plt_files[usms_plt_files != "null"]
  plant_files <- get_in_files(
    in_dir_or_files = file.path(workspace, "plant"),
    kind = "plt"
  )
  plant_idx <- usms_plt_files %in% basename(plant_files)
  full_plant_files <- file.path(workspace, "plant", usms_plt_files[plant_idx])

  usms_plt_files <- usms_plt_files[!plant_idx]

  if (!is.null(javastics)) {
    plant_files <- get_in_files(
      in_dir_or_files = file.path(javastics, "plant"),
      kind = "plt"
    )

    plant_idx <- usms_plt_files %in% basename(plant_files)

    # Combining javastics and workspace plant files
    full_plant_files <- c(
      full_plant_files,
      file.path(javastics, "plant", usms_plt_files[plant_idx])
    )
  }

  if (length(full_plant_files) > 0) {
    # For creating a sub-directory in workspace for upgraded plant files
    plant_out_dir <- file.path(out_dir, "plant")
    if (!dir.exists(plant_out_dir)) {
      dir.create(plant_out_dir)
    }

    upgrade_plt_xml(
      file = full_plant_files,
      out_dir = plant_out_dir,
      param_gen_file = par_gen,
      param_newform_file = par_new,
      ...,
      overwrite = overwrite
    )

    if (verbose) {
      message("*_plt.xml\n")
    }
  }

  # Other files types copy from the source workspace,
  # or from javastics "example"
  # dir for *.mod files if they do not exist in the workspace
  workspace_files_copy(
    workspace = workspace,
    out_dir = out_dir,
    javastics = javastics,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {
    message(paste0(
      "-----------------------------------\n",
      "Files upgrade and copy is complete.\n"
    ))
  }
}


#' Get the minimal upgradable file version
#'
#' @returns A numerical major version
#' @keywords internal
#' @noRd
#'
get_min_major_version <- function() {
  get_major_version(get_version_num(9))
}

#' Checking if the file version is an upgradable version
#'
#' @description
#' If the file major version is too low an arror is raised
#'
#' @returns A numerical major version
#' @keywords internal
#' @noRd
#'
file_upgradable_version <- function(file, param_gen_file) {
  # Treating a files list
  if (length(file) > 1) {
    major_versions <- unique(unlist(lapply(file, function(x) {
      file_upgradable_version(
        file = x,
        param_gen_file = param_gen_file
      )
    })))
    if (length(major_versions) > 1)
      stop(
        "No possible upgrade, all files versions are not identical: ",
        paste(major_versions, collapse = ", ")
      )
    return(major_versions)
  }

  # Getting information about the file initial version
  file_version <- get_xml_file_version(file, param_gen_file = param_gen_file)
  version_num <- get_version_num(file_version)

  # get major version
  major_version <- get_major_version(version_num)

  if (major_version < 9) {
    stop(
      file,
      ": the file is not upgradable, its major version is too old: ",
      major_version
    )
  }
  major_version
}
