get_in_files <- function(in_dir_or_files, kind) {
  files_patterns <- list(
    ini = "_ini",
    sta = "_sta",
    plt = "_plt",
    tec = "_tec",
    gen = "_gen",
    newform = "_newform",
    sols = "^sols",
    usms = "^usms",
    obs = "\\.obs$"
  )

  if (!kind %in% names(files_patterns)) {
    stop("file kind error: ", kind)
  }

  if (kind == "obs") {
    file_pattern <- files_patterns[[kind]]
  } else {
    file_pattern <- paste0(files_patterns[[kind]], "\\.xml$")
  }

  if (dir.exists(in_dir_or_files)) {
    files_list <- list.files(
      pattern = file_pattern,
      path = in_dir_or_files,
      full.names = TRUE
    )
  } else {
    # files path: add grep for filtering
    files_list <- grep(
      pattern = file_pattern,
      x = in_dir_or_files,
      value = TRUE
    )
    exist <- file.exists(files_list)
    if (!all(exist)) {
      warning(
        "Some files do not exist",
        paste(files_list[!exist], collapse = ", ")
      )
      files_list <- files_list[exist]
    }
  }

  files_list
}

get_param_gen_file <- function(
  file,
  workspace,
  javastics = NULL
) {
  if (!file %in% c("param_gen.xml", "param_newform.xml")) {
    stop(file, "is not a gerenal parameters file name !")
  }

  par_file <- file.path(workspace, file)

  exists_in_workspace <- file.exists(par_file)

  if (exists_in_workspace) {
    attr(par_file, "where") <- "workspace"
    return(par_file)
  }

  if (is.null(javastics)) {
    stop(
      "JavaSTICS path is needed as as input argument\n",
      file,
      " has not been found in ",
      workspace
    )
  }

  par_file <- file.path(javastics, "config", file)

  exists_in_javastics <- file.exists(par_file)

  if (exists_in_javastics) {
    attr(par_file, "where") <- "javastics"
    return(par_file)
  }

  stop(
    file,
    " has not been found neither in ",
    javastics,
    " nor in",
    workspace
  )
}

set_xml_file_version <- function(xml_doc, new_version) {
  # If an xml document is given file must not be null
  if (!inherits(xml_doc, "xml_document")) {
    stop(
      "xml_doc must be an xml_document object ",
      "(i.e. from `xml_doc <- SticsRFiles:::xmldocument(file)`)"
    )
  }
  # Adding version to detect if the file have been previously updated to 10.0
  ver <- get_version_num(new_version, numeric = FALSE)
  names(ver) <- "version"
  root_path <- paste0("/", XML::xmlName(XML::xmlRoot(xml_doc@content)))
  att_value <- get_attrs_values(
    xml_doc,
    path = root_path,
    attr_list = "version"
  )

  if (length(att_value) == 0) {
    add_attrs(xml_doc, path = root_path, named_vector = ver)
    return(invisible())
  }

  # Checking file version
  if (!is.null(att_value) && att_value[, "version"] == ver) {
    warning("The file is already in version ", new_version)
    return(invisible(xml_doc))
  }

  set_attrs_values(
    xml_doc,
    path = root_path,
    attr_name = "version",
    values_list = ver
  )

  add_attrs(xml_doc, path = root_path, named_vector = ver)

  invisible(xml_doc)
}

# TODO : see existing get_xml_stics_version, to be merged or replaced with
# this one and see what to do in gen_*_doc functions using templates ...
get_xml_file_version <- function(xml_file_or_doc, param_gen_file = NULL) {
  xml_doc <- get_xml_doc(xml_file_or_doc)
  xml_root_name <- XML::xmlName(XML::xmlRoot(xml_doc@content))

  att <- get_attrs(xml_doc, path = paste0("/", xml_root_name))

  # "version" attribute added in files from version 10
  # for 10 and above, early return
  if ("version" %in% colnames(att)) {
    version_string <- get_version_string(att[, "version"])
    return(version_string)
  }

  # Global detection of the version based for the moment on the
  # param_gen.xml file content
  if (xml_root_name != "fichierpar" && is.null(param_gen_file)) {
    warning("STICS version corresponding to the XML file was not detected.\n")
    return()
  }

  # Getting specific parameters attached to versions from param_gen file
  if (xml_root_name == "fichierpar") {
    param_gen_doc <- xml_doc
  } else {
    param_gen_doc <- get_xml_doc(param_gen_file)
  }
  # Using markers of versions
  codesnow <- get_param_value(param_gen_doc, param_name = "codesnow")
  tmin_mineralisation <- get_param_value(
    param_gen_doc,
    param_name = "tmin_mineralisation"
  )

  # Vector of existence in the doc
  is_null <- c(is.null(codesnow), is.null(tmin_mineralisation))

  # none of them exist
  if (all(is_null)) {
    return("V8.5")
  }

  # only codesnow exists
  if (!is_null[1] && is_null[2]) {
    return("V9.0")
  }

  # both exist
  # How to make a distinction between 9.1 and 9.2 ?
  # using the STICS exe ???
  # For the moment returning a vector of versions !!!
  if (all(!is_null)) {
    return(c("V9.1", "V9.2"))
  }

  warning("Unknown version !")
  return()
}

get_xml_doc <- function(xml_file_or_doc) {
  type_id <- c("character", "xml_document") %in% class(xml_file_or_doc)

  if (!any(type_id)) {
    stop(
      "xml_file_or_doc: not a valid input,
                          must be a file path or an xml_document object!"
    )
  }

  if (type_id[1] && !file.exists(xml_file_or_doc)) {
    stop(
      xml_file_or_doc,
      ": does not exist!"
    )
  }

  if (type_id[1]) {
    return(xmldocument(xml_file_or_doc))
  }

  if (type_id[2]) {
    return(xml_file_or_doc)
  }
}


node_exist <- function(xml_file_or_doc, xpath) {
  xml_doc <- get_xml_doc(xml_file_or_doc)

  nodes <- get_nodes(xml_doc, xpath)

  if (is.null(nodes)) {
    return(FALSE)
  }

  unlist(lapply(nodes, function(x) !is.null(x)))
}


write_xml_file <- function(xml_doc, file, overwrite = FALSE) {
  if (file.exists(file) && (!overwrite)) {
    warning(file, ": \nalready exists, consider setting overwrite to TRUE")
    return(invisible(FALSE))
  }

  save_xml_doc(xml_doc, file)
  return(invisible(TRUE))
}

#' Check if the versions for upgrading xml files are compatible
#'
#' @param xml_doc An SticsRFiles xml_doc object
#' @param from_version STICS starting version
#' @param target_version STICS target version
#' @param verbose   logical, TRUE for displaying an information message
#' FALSE otherwise (default)
#'
#' @return a logical value TRUE if changing the version has been successful,
#' or FALSE otherwise; the file initial major version is identical to the
#' target one.
#'
#' @description
#' Defining if the starting version to use for files upgrading process
#' is compatible with the target version.
#' Versions may be given either as character strings (i.e. "V9.2")
#' or numerical value (i.e. 9.2)
#'
#' @keywords internal
#' @noRd
#'
check_and_upgrade_xml_version <- function(
  xml_doc,
  from_version,
  target_version,
  verbose = FALSE
) {
  # Checking if target version is supported
  # raising an error if not!
  check_version(target_version)
  target_version_num <- get_version_num(target_version)
  from_version_major <- get_major_version(get_version_num(from_version))
  target_version_major <- get_major_version(target_version_num)

  if (target_version_major > from_version_major + 1) {
    stop(
      "The target version ",
      target_version,
      " must be only one major version higher than the initial version ",
      from_version
    )
  }

  # checking actual version consistency between from_version
  # and file version
  file_version_major <- get_major_version(
    get_version_num(
      get_xml_file_version(
        xml_doc
      )
    )
  )
  # the file version and the target version are the same!
  if (from_version_major == target_version_major) {
    if (verbose)
      message(
        "The initial file version is already a ",
        from_version_major,
        "version!"
      )
    return(FALSE)
  }

  if (from_version_major != file_version_major) {
    stop(
      "The file major version is not consistent with the given",
      "initial version!",
      "it must be a",
      paste0(from_version_major, ".x")
    )
  }
  # Setting new file STICS version
  set_xml_file_version(xml_doc, new_version = target_version_num)
  TRUE
}
