#' Upgrading _ini.xml file(s) to a newer version
#'
#' @param file Path of an initialisation (*_ini.xml) file or a vector of
#' @param out_dir Output directory path of the generated files
#' @param stics_version Name of the Stics version (VX.Y format)
#' @param target_version Name of the Stics version to upgrade files to (VX.Y format)
#' @param check_version Perform version consistency with in stics_version input
#' with the file version and finally checking if the upgrade is possible
#' allowed to the target_version. If TRUE, param_gen_file is mandatory.
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#' @param ... Additional input arguments
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#' upgrade_ini_xml(file = "/path/to/_ini.xml",
#'                 out_dir = "/path/to/directory")
#'
upgrade_ini_xml <- function(file,
                            out_dir,
                            stics_version = "V9.2",
                            target_version = "V10.0",
                            check_version = TRUE,
                            param_gen_file = NULL,
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
    if(is.null(param_gen_file)) stop("param_gen_file must be provided! ")

    min_version <- get_version_num("V9.1")

    # extracting or detecting the Stics version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- check_xml_file_version(file[1],
                                           stics_version,
                                           param_gen_file = param_gen_file)

    if (!file_version) {
      stop("The input version ",stics_version,
           " does not match file version ",
           attr(file_version,"version")," \n",file[1] )
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
    lapply(file, function(x) upgrade_ini_xml(file = x,
                                             out_dir = out_dir,
                                             stics_version = stics_version,
                                             target_version = target_version,
                                             check_version = check_version,
                                             param_gen_file = param_gen_file,
                                             overwrite = overwrite,
                                             check_dir = check_dir)
    )
    return(invisible())
  }


  # Loading the old xml file
  old_doc <- xmldocument(file = file)

  # Setting file stics version
  set_xml_file_version(old_doc, new_version = target_version, overwrite = overwrite)

  # Keeping old values
  rm_names <- c("masec0","QNplante0", "resperenne0")
  old_values <- get_param_value(old_doc, rm_names)

  # Removing useless nodes
  rm_nodes <- unlist(lapply(rm_names, function(x)
    unlist(getNodeS(docObj = old_doc,
                    path = paste0("//",x)))))
  lapply(rm_nodes, function(x) removeNodes(x))


  # Getting new option node
  new_node <- xmlParseString(
    '<option choix="2" nom="Simulation of Nitrogen and Carbon reserves" nomParam="code_acti_reserve">
	<choix code="1" nom="yes">
		<maperenne0>0</maperenne0>
		<QNperenne0>0</QNperenne0>
		<masecnp0>0</masecnp0>
		<QNplantenp0>0</QNplantenp0>
	</choix>
	<choix code="2" nom="no">
		<masec0>0</masec0>
		<QNplante0>0</QNplante0>
		<resperenne0>0</resperenne0>
	</choix>
 </option>',
    addFinalizer = TRUE)


  # Getting zrac0 node
  prev_sibling <- unlist(getNodeS(old_doc, "//zrac0"))

  # Adding new node
  lapply(prev_sibling, function(x) addSibling(x, xmlClone(new_node)))

  # Error ...
  # In set_param_value(xml_doc, param_name = param_name[p], param_value = param_value[[p]],  :
  #                      Parameters names and list of values are no of the same length !
  # Restoring values for masec0, QNplante0, resperenne0 from old ones
  #set_param_value(old_doc, param_name = rm_names, param_value = old_values)

  # Writing to file _ini.xml
  out_ini <- file.path(out_dir, basename(file))
  write_xml_file(old_doc, out_ini, overwrite)


  set_param_xml(out_ini,
                param_name = rm_names,
                old_values,
                overwrite = TRUE)



  free(old_doc@content)
  invisible(gc(verbose = FALSE))

}
