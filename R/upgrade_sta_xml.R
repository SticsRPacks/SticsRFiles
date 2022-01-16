#' Upgrading _sta.xml file(s) to a newer version
#'
#' @param file Path of a station (*_sta.xml) file or a vector of
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param out_dir Output directory path of the generated files
#' @param stics_version Name of the Stics version (VX.Y format)
#' @param target_version Name of the Stics version to upgrade files to (VX.Y format)
#' @param check_version Perform version consistency with in stics_version input
#' with the file version and finally checking if the upgrade is possible
#' allowed to the target_version. If TRUE, param_gen_file is mandatory.
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#' @param ... Additional input arguments
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#' upgrade_sta_xml(file = "/path/to/_sta.xml",
#'                 param_gen_file = "/path/to/param_gen.xml"
#'                 out_dir = "/path/to/directory")
#'
upgrade_sta_xml <- function(file,
                            param_gen_file,
                            out_dir,
                            stics_version = "V9.2",
                            target_version = "V10.0",
                            check_version = TRUE,
                            overwrite = FALSE,
                            ...) {

  # for verifying output dir existence
  check_dir <- TRUE
  args <- list(...)
  if ("check_dir" %in% names(args)) check_dir <- args$check_dir
  if (check_dir) {
    if (!dir.exists(out_dir)) dir.create(out_dir)
    # for checking only once when multiple files are treated !
    check_dir <- FALSE
  }

  # checking version
  if (check_version) {

    min_version <- get_version_num("V9.1")

    # extracting or detecting the Stics version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- check_xml_file_version(file[1],
                                           stics_version,
                                           param_gen_file = param_gen_file)

    if (!file_version) {
      stop("The input version ",stics_version,
           " does not match file version ",
           attr(file_version,"version")," \n",file[1])
    }

    # Compatibility checks between version and update to target_version
    ver_num <- get_version_num(stics_version)
    if (ver_num < min_version) stop("Files from the version ", stics_version,
                                    " cannot be converted to the version ", target_version)


    # for checking only once when multiple files are treated !
    check_version <- FALSE
  }

  # Treating a files list
  if (length(file) > 1 ) {
    lapply(file, function(x) upgrade_sta_xml(file = x,
                                             param_gen_file = param_gen_file,
                                             out_dir = out_dir,
                                             stics_version = stics_version,
                                             target_version = target_version,
                                             check_version = check_version,
                                             overwrite = overwrite,
                                             check_dir = check_dir)
    )
    return(invisible())
  }


  if (!file.exists(file)) {
    warning("Unknown file: ", file)
    return(invisible())
  }

  # Loading xml file
  old_doc <- xmldocument(file = file)

  # Setting file stics version
  set_xml_file_version(old_doc, new_version = target_version, overwrite = overwrite)

  # Getting old concrr value
  concrr <- get_param_xml(param_gen_file, "concrr")$param_gen.xml$concrr


  # Getting new parameter
  concrr_node <- xmlParseString(
    '<param format="real" max="3.0" min="0.0" nom="concrr">0.02000</param>',
    addFinalizer = TRUE)

  # Getting the preceeding sibling node
  prev_sibling <- getNodeS(old_doc, "//*[@nom='NH3ref']")[[1]]
  addSibling(node = prev_sibling, xmlClone(concrr_node), after = TRUE)

  # Setting concrr value
  set_param_value(old_doc,param_name = "concrr", param_value = concrr)

  # Writing to file _sta.xml
  write_xml_file(old_doc, file.path(out_dir, basename(file)), overwrite = overwrite )

  free(old_doc@content)
  invisible(gc(verbose = FALSE))
}
