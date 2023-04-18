#' Upgrading XML files of a JavaStics workspace directory
#' to a newer Stics version format
#'
#' @param workspace Path of a JavaStics workspace
#' @param javastics Path of JavaStics
#' @param out_dir Output directory of the generated files
#' @param stics_version Name of the Stics version (VX.Y format)
#' @param target_version Name of the Stics version to upgrade files
#' to  (VX.Y format)
#' @param plant logical (optional), TRUE for upgrading plant files if a "plant"
#' sub-directory of workspace exists, FALSE otherwise
#' @param overwrite logical (optional),
#' TRUE for overwriting files if they exist, FALSE otherwise
#' @param ... Additional input arguments
#'
#' @export
#'
#' @details
#' - See SticsRFiles::get_stics_versions_compat() for listing versions
#' - If general parameters files exist in `workspace`, they are also upgraded.
#' In that case, residues parameters values are kept and might not be adapted
#' to the target model version.
#' - Weather data and observations files are fully copied to `out_dir`
#'
#' @examples
#' \dontrun{
#' upgrade_workspace_xml(
#'   workspace = "/path/to/JavaSTICS/workspace",
#'   javastics = "/path/to/JavaSTICS/folder",
#'   out_dir = "/path/to/an/output/directory"
#' )
#' }
upgrade_workspace_xml <- function(workspace,
                                  javastics,
                                  out_dir,
                                  stics_version = "V9.2",
                                  target_version = "V10.0",
                                  plant = FALSE,
                                  overwrite = FALSE,
                                  ...) {


  # For testing if files are upgradable
  check_version <- FALSE
  verbose <- TRUE
  upgr_par_gen <- FALSE
  upgr_par_new <- FALSE

  args <- list(...)
  if ("check_version" %in% names(args)) check_version <- args$check_version
  if ("verbose" %in% names(args)) verbose <- args$verbose

  min_version <- get_version_num("V9.1")

  # Getting param_gen.xml path
  par_gen <- get_param_gen_file(type = "param_gen.xml", workspace, javastics)
  if (is.null(par_gen)) {
    stop(
      "param_gen.xml: file not found in\n",
      workspace, " or ", javastics, "directory! "
    )
  }
  if (attr(par_gen, "where") == "workspace") upgr_par_gen <- TRUE

  # Extracting or detecting the Stics version corresponding to the xml file
  # based on param_gen.xml file content
  file_version <- check_xml_file_version(par_gen, stics_version)
  if (!file_version) {
    stop(
      "The input version ", stics_version,
      " does not match file version ",
      attr(file_version, "version"), " \n", par_gen
    )
  }


  # Compatibility checks between version and upgrade to target_version
  ver_num <- get_version_num(stics_version)
  if (ver_num < min_version) {
    stop(
      "Files from the version ", stics_version,
      " cannot be converted to the version ", target_version
    )
  }

  #----------------------------------------------------------------------------
  # TODO: add a call for getting tags
  # check_upgrade_from_to_versions to enable
  # switch in each upgrade function according the from/to correspondence.
  #----------------------------------------------------------------------------

  # Testing the workspace dir to be converted
  if (!dir.exists(workspace) ||
    !file.exists(file.path(workspace, "usms.xml"))) {
    stop(
      workspace,
      ": the directory does not exist or is not a JavaStics workspace !"
    )
  }

  # Just in case, creating the target directory
  if (!dir.exists(out_dir)) dir.create(out_dir)

  # Testing the JavaStics dir
  if (!dir.exists(javastics) ||
    !file.exists(file.path(javastics, "JavaStics.exe"))) {
    stop(
      javastics,
      " : the directory does nor exist or is not a JavaStics one !"
    )
  }


  if (verbose) {
    cat(paste(
      "Upgrading files from version", stics_version, "to",
      target_version, "\n"
    ))
    cat(paste("From: ", workspace, "\n"))
    cat(paste("To: ", out_dir, "\n"))
    cat("-----------------------------------\n")
  }

  # Converting param_gen.xml
  if (upgr_par_gen) {
    upgrade_param_gen_xml(
      file = par_gen,
      out_dir = out_dir,
      stics_version = stics_version,
      target_version = target_version,
      check_version = check_version,
      overwrite = overwrite
    )

    if (verbose) {
      cat("param_gen.xml\n")
    }
  }


  # Getting param_newform.xml path
  par_new <- get_param_gen_file(
    type = "param_newform.xml",
    workspace, javastics
  )
  if (is.null(par_new)) {
    stop(
      "param_newform.xml: file not found in\n",
      workspace, " or ", javastics, "directory! "
    )
  }
  if (attr(par_new, "where") == "workspace") upgr_par_new <- TRUE

  # Converting param_newform.xml
  if (upgr_par_new) {
    upgrade_param_newform_xml(
      file = par_new,
      out_dir = out_dir,
      param_gen_file = par_gen,
      stics_version = stics_version,
      target_version = target_version,
      check_version = check_version,
      overwrite = overwrite
    )

    if (verbose) {
      cat("param_new_form.xml\n")
    }
  }


  # Converting usms.xml file
  usms <- file.path(workspace, "usms.xml")

  upgrade_usms_xml(
    file = usms,
    out_dir = out_dir,
    param_gen_file = par_gen,
    obs_dir = workspace,
    stics_version = stics_version,
    target_version = target_version,
    check_version = check_version,
    overwrite = overwrite
  )

  if (verbose) {
    cat("usms.xml\n")
  }

  # Converting sols.xml file
  sols <- file.path(workspace, "sols.xml")

  upgrade_sols_xml(
    file = sols,
    out_dir = out_dir,
    param_gen_file = par_gen,
    stics_version = stics_version,
    target_version = target_version,
    check_version = check_version,
    overwrite = overwrite
  )

  if (verbose) {
    cat("sols.xml\n")
  }

  # Converting station files (*_sta.xml)
  sta_files <- get_in_files(in_dir_or_files = workspace, kind = "sta")

  upgrade_sta_xml(
    file = sta_files,
    out_dir = out_dir,
    param_gen_file = par_gen,
    stics_version = stics_version,
    target_version = target_version,
    check_version = check_version,
    overwrite = overwrite,
    check_dir = FALSE
  )

  if (verbose) {
    cat("*_sta.xml\n")
  }

  # Converting initialisation files (*_ini.xml)
  ini_files <- get_in_files(in_dir_or_files = workspace, kind = "ini")

  upgrade_ini_xml(
    file = ini_files,
    out_dir = out_dir,
    param_gen_file = par_gen,
    stics_version = stics_version,
    target_version = target_version,
    check_version = TRUE,
    overwrite = overwrite,
    check_dir = FALSE
  )

  if (verbose) {
    cat("*_ini.xml\n")
  }

  # Converting crop management files (*_tec.xml)
  tec_files <- get_in_files(in_dir_or_files = workspace, kind = "tec")

  upgrade_tec_xml(
    file = tec_files,
    out_dir = out_dir,
    param_newform_file = par_new,
    param_gen_file = par_gen,
    stics_version = stics_version,
    target_version = target_version,
    check_version = check_version,
    overwrite = overwrite,
    check_dir = FALSE
  )

  if (verbose) {
    cat("*_tec.xml\n")
  }

  # Copying *.mod files (for model outputs)
  stat <- file.copy(
    from = list.files(path = workspace, full.names = TRUE, pattern = "*.mod"),
    to = out_dir, overwrite = overwrite
  )

  if (verbose) {
    cat("Copying *.mod files.\n")
  }

  # TODO: see how to manage variables names checks in *.mod files
  # Probably, the new JavaStics path may be added as a new function argument
  # for getting information on output variables
  # (use get_var_info with the appropriate version string)


  # Copying observation files
  # all .obs files deteted in workspace,
  # not only according to usms names, because now obs files names are
  # references in the usms.xml file (i.e. not fixed a priori)
  stat <- file.copy(
    from = list.files(path = workspace, full.names = TRUE, pattern = "*.obs"),
    to = out_dir, overwrite = overwrite
  )

  if (verbose) {
    cat("Copying *.obs files.\n")
  }

  # Copying weather data files
  # Note: for the moment, all files with a numerical extension
  # (i.e. year, as .1996) are taken into account because when USM are
  # defined over 2 successive years, files are not explicitly mentioned
  # in the usms.xml file.

  weather_files <-
    list.files(workspace, full.names = TRUE, pattern = "\\.[0-9]")

  stat <- file.copy(
    from = weather_files,
    to = out_dir, overwrite = overwrite
  )

  if (!all(stat)) {
    warning("Error when copying file(s): ",
            paste(weather_files[stat], collapse = ", "))
  }

  if (verbose) {
    cat("Copying weather files.\n")
  }



  # Upgrading plant files
  # if a plant sub directory exists in workspace
  plant_files <- get_in_files(
    in_dir_or_files = file.path(workspace, "plant"),
    kind = "plt"
  )

  if (length(plant_files) > 0) {
    if (verbose) {
      cat("*_plt.xml\n")
    }

    # For creating a sub-directory in workspace for upgraded plant files
    plant_out_dir <- file.path(out_dir, "plant")
    if (!dir.exists(plant_out_dir)) dir.create(plant_out_dir)

    upgrade_plt_xml(
      file = plant_files,
      out_dir = plant_out_dir,
      param_gen_file = par_gen,
      param_newform_file = par_new,
      stics_version = stics_version,
      target_version = target_version,
      check_version = check_version,
      overwrite = overwrite,
      check_dir = FALSE
    )
  }

  if (verbose) {
    cat("-----------------------------------\n")
  }
  cat("Files upgrade and copy is complete.\n")
}
