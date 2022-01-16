#' Upgrading XML files of a JavaStics workspace directory
#' to a newer Stics version format
#'
#' @param workspace Path of a JavaStics workspace
#' @param javastics Path of JavaStics
#' @param out_dir Output directory of the generated files
#' @param stics_version Name of the Stics version (VX.Y format)
#' @param target_version Name of the Stics version to upgrade files to  (VX.Y format)
#' @param overwrite logical (optional),
#' TRUE for overwriting files if they exist, FALSE otherwise
#' @param ... Additional input arguments
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#' upgrade_workspace_xml(workspace = "/path/to/javastics/workspace",
#'                       javastics = "/path/to/javastics/directory",
#'                       out_dir = "/path/to/an/output/directory")
#'
upgrade_workspace_xml <- function(workspace,
                                  javastics,
                                  out_dir,
                                  stics_version = "V9.2",
                                  target_version = "V10.0",
                                  overwrite = FALSE,
                                  ...) {

  # TODO: to put elsewhere for getting compat for a list of versions !
  # v <- list("8.5","9.0",c("9.1","9.2))
  # c <- c(FALSE,FALSE,TRUE)
  # list(NULL, NULL, "V10.0")

  # For testing if files are upgradable
  check_version <- FALSE
  args <- list(...)
  if ("check_version" %in% names(args)) check_version <- args$check_version

  min_version <- get_version_num("V9.1")

  # Getting param_gen.xml path
  par_gen <- get_param_gen_file(type = "param_gen.xml", workspace, javastics )
  if(is.null(par_gen)) stop("param_gen.xml: file not found in\n",
                            workspace, " or ", javastics, "directory! ")

  # Extracting or detecting the Stics version corresponding to the xml file
  # based on param_gen.xml file content
  file_version <- check_xml_file_version(par_gen, stics_version)
  if (!file_version) {
    stop("The input version ",stics_version,
         " does not match file version ",
         attr(file_version,"version")," \n",par_gen )
  }


  # Compatibility checks between version and upgrade to target_version
  ver_num <- get_version_num(stics_version)
  if (ver_num < min_version) stop("Files from the version ", stics_version,
                                  " cannot be converted to the version ", target_version)

  #----------------------------------------------------------------------------
  # TODO: add a call for getting tags
  # check_upgrade_from_to_versions to enable
  # switch in each upgrade function according the from/to correspondence.
  #----------------------------------------------------------------------------

  # Testing the workspace dir to be converted
  if (!dir.exists(workspace) || !file.exists(file.path(workspace, "usms.xml")))
    stop(workspace, ": the directory does not exist or is not a javaStics workspace !")

  # Just in case, creating the target directory
  if (!dir.exists(out_dir)) dir.create(out_dir)

  # Testing the JavaStics dir
  if (!dir.exists(javastics) || !file.exists(file.path(javastics, "JavaStics.exe")))
    stop(javastics, " : the directory does nor exist or is not a JavaStics one !")


  # Searching for general parameter files (workspace ou JavaStics)
  # in the workspace to be converted, or in JavaStics config directory
  # Getting param_gen.xml path, already done at the beginning !
  # par_gen <- get_param_gen_file(type = "param_gen.xml", workspace, javastics )
  # if(is.null(par_gen)) stop("param_gen.xml: file not found in\n",
  #                           workspace, " or ", javastics, "directory! ")
  # Converting param_gen.xml
  upgrade_param_gen_xml(file = par_gen,
                        out_dir = out_dir,
                        stics_version = stics_version,
                        target_version = target_version,
                        check_version = check_version,
                        overwrite = overwrite)

  # Getting param_newform.xml path
  par_new <- get_param_gen_file(type = "param_newform.xml", workspace, javastics )
  if(is.null(par_new)) stop("param_newform.xml: file not found in\n",
                            workspace, " or ", javastics, "directory! ")

  # Converting param_newform.xml
  upgrade_param_newform_xml(file = par_new,
                            out_dir = file.path(out_dir),
                            stics_version = stics_version,
                            target_version = target_version,
                            check_version = check_version,
                            param_gen_file = par_gen,
                            overwrite = overwrite)


  # Converting usms.xml file
  usms <- file.path(workspace, "usms.xml")

  upgrade_usms_xml(file = usms,
                   out_dir = out_dir,
                   obs_dir = workspace,
                   stics_version = stics_version,
                   target_version = target_version,
                   check_version = check_version,
                   param_gen_file = par_gen,
                   overwrite = overwrite)



  # Converting sols.xml file
  sols <- file.path(workspace, "sols.xml")

  upgrade_sols_xml(file = sols,
                   out_dir = out_dir,
                   stics_version = stics_version,
                   target_version = target_version,
                   check_version = check_version,
                   param_gen_file = par_gen,
                   overwrite = overwrite)



  # Converting station files (*_sta.xml)
  sta_files <- get_in_files(in_dir_or_files = workspace, kind = "sta")

  upgrade_sta_xml(file = sta_files,
                  param_gen_file = par_gen,
                  out_dir = out_dir,
                  stics_version = stics_version,
                  target_version = target_version,
                  check_version = check_version,
                  overwrite = overwrite,
                  check_dir = FALSE)


  # Converting initialisation files (*_ini.xml)
  ini_files <- get_in_files(in_dir_or_files = workspace, kind = "ini")

  upgrade_ini_xml(file = ini_files,
                  out_dir = out_dir,
                  stics_version = stics_version,
                  target_version = target_version,
                  check_version = check_version,
                  param_gen_file = par_gen,
                  overwrite = overwrite,
                  check_dir = FALSE)


  # Converting crop management files (*_tec.xml)
  tec_files <- get_in_files(in_dir_or_files = workspace, kind = "tec")

  upgrade_tec_xml(file = tec_files,
                  param_newform_file = par_new,
                  out_dir = out_dir,
                  stics_version = stics_version,
                  target_version = target_version,
                  check_version = check_version,
                  param_gen_file = par_gen,
                  overwrite = overwrite,
                  check_dir = FALSE)


  # Copying *.mod files (for model outputs)
  stat <- file.copy(from = list.files(path = workspace, full.names = TRUE, pattern = "*.mod"),
                    to = out_dir, overwrite = overwrite)

  # TODO: see how to manage variables names checks in *.mod files
  # Probably, the new JavaStics path may be added as a new function argument
  # for getting information on output variables
  # (use get_var_info with the appropriate version string)


  # Copying observation files
  # all .obs files deteted in workspace,
  # not only according to usms names, because now obs files names are
  # references in the usms.xml file (i.e. not fixed a priori)
  stat <- file.copy(from = list.files(path = workspace, full.names = TRUE, pattern = "*.obs"),
                    to = out_dir, overwrite = overwrite)


  # Copying weather data files
  # Note: for the moment, all files with a numerical extension
  # (i.e. year, as .1996) are taken into account because when USM are
  # defined over 2 successive years, files are not explicitly mentioned
  # in the usms.xml file.
  stat <- file.copy(from = list.files(workspace, full.names = TRUE, pattern = "\\.[0-9]"),
                    to = out_dir, overwrite = overwrite)

}



