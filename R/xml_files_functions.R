get_in_files <- function(in_dir_or_files, kind) {

  files_patterns <- list(ini="_ini", sta="_sta",
                         plt="_plt", tec="_tec",
                         gen="_gen", newform="_newform",
                         sols="^sols", usms="^usms", obs="\\.obs$")

  if (!kind %in% names(files_patterns)) stop("file kind error: ", kind)

  if (kind == "obs") {
    file_pattern <- files_patterns[[kind]]
  } else {
    file_pattern <- paste0(files_patterns[[kind]],"\\.xml$")
  }

  if (dir.exists(in_dir_or_files)) {
    files_list <- list.files(pattern = file_pattern, path = in_dir_or_files, full.names = TRUE)
  } else {
    # files path: add grep for filtering
    files_list <- grep(pattern = file_pattern, x = in_dir_or_files, value = TRUE)
    exist <- file.exists(files_list)
    if (!all(exist)) {
      warning("Some files do not exist", paste(files_list[!exist], collapse = ", "))
      files_list <- files_list[exist]
    }
  }

  files_list
}

get_param_gen_file <- function(type = c("param_gen.xml", "param_newform.xml"),
                               workspace_dir,
                               javastics_dir = NULL) {

  par_file <- file.path(workspace_dir, type)

  if (file.exists(par_file)) {
    attr(par_file, "where") <- "workspace"
    return(par_file)
  }

  if (is.null(javastics_dir)) {
    warning("JavaStics path must be given as input argument\n",
            type, " has not been found in ",workspace_dir)
    return()
  }

  par_file <- file.path(javastics_dir, "config", type)

  if (file.exists(par_file)) {
    attr(par_file, "where") <- "javastics"
    return(par_file)
  }

  warning(type, " has not been found in ",javastics_dir)

  return()
}

to_xml_version <- function(stics_version) {

  err <- FALSE

  if (is.numeric(stics_version)) stics_version <- as.character(stics_version)

  if (!grepl(pattern = "\\.", x = stics_version)) stics_version <- paste0(stics_version,".0")

  numbers <- grepl(pattern = "[0-9]", stics_version)

  # no numbers in version
  if (!numbers) err <- TRUE

  char_no_v <- grepl(pattern = "[a-u w-z A-U W-Z]", stics_version)

  # Wrong character in version
  if(char_no_v) err <- TRUE

  # No dot in version

  if (grepl(pattern = "\\.$", x = stics_version)) err <- TRUE


  if (err){
    warning("Version must be X.Y, or VX.Y, or vX.Y")
    return()
  }

  as.character(get_version_num(stics_version = stics_version, numeric = FALSE))
}


#set_xml_stics_version <- function(xml_file_or_doc, new_version="V10.0", overwrite = FALSE) {
set_xml_file_version <- function(xml_file_or_doc, new_version="V10.0", overwrite = FALSE) {
  # Getting xmlDocument object
  xml_doc <- get_xml_doc(xml_file_or_doc)

  # Adding version to detect if the file have been previously updated to 10.0
  ver <- to_xml_version(new_version)
  names(ver) <- "version"
  root_path <- paste0("/",xmlName(xmlRoot(xml_doc@content)))
  att <- getAttrs(xml_doc, path = root_path)

  # Checking file version
  if (!is.null(att) && att[,"version"] == new_version && !overwrite) {
    stop(paste("The version has already been updated to Stics version",
               new_version))
  }

  addAttrs(xml_doc, path = root_path, named_vector = ver)
}

# TODO : see existing get_xml_stics_version, to be merged or replaced with
# this one and see waht  to do in gen_*_doc functions using templates ...
#get_xml_stics_version <- function(xml_file_or_doc, param_gen_file = NULL ) {
get_xml_file_version <- function(xml_file_or_doc, param_gen_file = NULL ) {

  xml_doc <- get_xml_doc(xml_file_or_doc)
  xml_root_name <- xmlName(xmlRoot(xml_doc@content))

  att <- getAttrs(xml_doc, path = paste0("/",xml_root_name))

  if ("version" %in% colnames(att)) {
    version_string <- get_version_string(att[,"version"])
    return(version_string)
  }

  # Global detection of the version based for the moment on the param_gen.xml file content
  if (xml_root_name!= "fichierpar" && is.null(param_gen_file)) {
    # stop("The Stics version corresponding to the XML file was not detected.\n",
    #      "The param_gen.xml path must be provided as second input!")
    warning("The Stics version corresponding to the XML file was not detected.\n")
    return()
  }

  # Getting specific parameters attached to versions from param_gen file
  if (xml_root_name == "fichierpar") {
    param_gen_doc <- xml_doc
  } else {
    param_gen_doc <- get_xml_doc(param_gen_file)
  }
  # Using markers of versions
  codesnow <- get_param_value(param_gen_doc,
                              param_name = "codesnow")
  tmin_mineralisation <- get_param_value(param_gen_doc,
                                         param_name = "tmin_mineralisation")

  # Vector of existence in the doc
  is_null <- c(is.null(codesnow), is.null(tmin_mineralisation))

  # none of them exist
  if (all(is_null)) return("V8.5")

  # only codesnow exists
  if (!is_null[1] && is_null[2]) return("V9.0")

  # both exist
  # How to make a distinction between 9.1 and 9.2 ?
  # using the stics exe ???
  # For the moment returning a vector of versions !!!
  if (all(!is_null)) return(c("V9.1","V9.2"))

  warning("Unknown version !")
  return()
}

# TODO: see *xml_file_version functions ...
#check_xml_stics_version <- function(xml_file_or_doc, version, param_gen_file = NULL) {
check_xml_file_version <- function(xml_file_or_doc, stics_version, param_gen_file = NULL) {
  #xml_version <- get_xml_stics_version(xml_file_or_doc, param_gen_file = param_gen_file)
  xml_version <- get_xml_file_version(xml_file_or_doc, param_gen_file = param_gen_file)

  r <- TRUE

  if(is.null(xml_version)) {
    return(FALSE)
  }

  if ( ! stics_version %in% xml_version) r <- FALSE

  attr(r,"version") <- xml_version
  return(r)

}

get_xml_doc <- function(xml_file_or_doc) {

  type_id <-  c("character", "xmlDocument") %in% class(xml_file_or_doc)

  if (!any(type_id)) stop("xml_file_or_doc: not a valid input, must be a file path or an xmlDocument object!")

  if (type_id[1] && !file.exists(xml_file_or_doc)) stop(xml_file_or_doc, ": does not exist!")

  if (type_id[1]) return(xmldocument(xml_file_or_doc))

  if (type_id[2]) return(xml_file_or_doc)


}


node_exist <- function(xml_file_or_doc, xpath) {
  xml_doc <-  get_xml_doc(xml_file_or_doc)

  nodes <- getNodeS(xml_doc, xpath)

  if (is.null(nodes)) return(FALSE)

  unlist(lapply(nodes, function(x) !is.null(x)))
}

# TODO: compare with SticsRFiles existing functions
# and evaluate if following func are useful or not
# get_nodes <- function(xml_doc, what, elt_name) {
#   # elt_name: name of "formalisme" or "option" or a vector of
#   if (!what %in% c("formalisme", "option")) {
#     warning("Unknown XML element: ", what)
#     return()
#   }
#
#   if (what == "formalisme") path_start <- "//formalisme[@nom='"
#
#   if (what == "option") path_start <- "//option[@nom='"
#
#   lapply(elt_name, function(x)
#     getNodeS(docObj = xml_doc,
#                            path = paste0(path_start,x,"']")))
#
# }
#
# remove_formalism <- function(xml_doc, elt_name) {
#   remove_nodes_from_doc(xml_doc = xml_doc,
#                         what = "formalisme",
#                         elt_name = elt_name)
# }
#
# remove_option <- function(xml_doc, elt_name) {
#   remove_nodes_from_doc(xml_doc = xml_doc,
#                         what = "option",
#                         elt_name = elt_name)
# }
#
# remove_nodes_from_doc <- function(xml_doc, what, elt_name) {
#
#   nodes_to_rm <- get_nodes(xml_doc = xml_doc,
#                            what = what,
#                            elt_name = elt_name)
#
#   lapply(nodes_to_rm, function(x) if (!is.null(x)) removeNodes(x))
#
# }
#
# add_formalism_to_doc <- function() {
#
# }
#
# add_option_to_doc <- function() {
#
# }
#
# add_node_to_doc <- function(xml_doc, parent_or_sibling, new_node) {
#
#   if (length(new_node)==1) new_node <- list(new_node)
#
#   what <- unlist(lapply(new_node, xmlName))
#   elt_name <- unlist(lapply(new_node, function(x) xmlAttrs(x)["nom"]),
#                      use.names = FALSE)
#
#
#   nodes_nb <- length(what)
#   nodes_from_doc <- vector(mode="list", length = length(what) )
#   for (i in 1:nodes_nb) {
#     nodes_from_doc[[i]] <- get_nodes(xml_doc = xml_doc,
#                                      what = what[i],
#                                      elt_name = elt_name[i])[[1]][[1]]
#   }
#   # getting NULL nodes, which don't exist in doc
#   nodes_from_doc <- nodes_from_doc[unlist(lapply(nodes_from_doc,is.null))]
# }
#
# reindent_xml_file <- function(file, out_file, overwrite = FALSE) {
#
#   write_xml_file(xml_doc = get_xml_doc(file), file = out_file, overwrite = overwrite)
#
# }

write_xml_file <- function(xml_doc, file, overwrite = FALSE) {

  if (file.exists(file) & !overwrite) {
    warning(file, ": \nalready exists, consider setting overwrite to TRUE")
    return(invisible(FALSE))
  }

  saveXmlDoc(xml_doc, file)
  return(TRUE)
}

# TODO: to be evaluated later, usefull ?
# check_update_from_to_versions <- function(stics_version, target_version = "V10.0") {
#
#   # TODO: to define for getting compat for a list of versions !
#   # v <- list("8.5","9.0",c("9.1","9.2))
#   # c <- c(FALSE,FALSE,TRUE)
#   # list(NULL, NULL, "V10.0")
#   # functions tag c("8_5", "9_0","9_1-2")
#   # TODO
#   # See for using the csv file collecting info about
#   # version taken into account in SticsRFiles
#
#   # Compatibility checks between version and update to target_version
#   check_version_compat(stics_version)
#
#   # check about the target version
#   # TODO: add check to test if version exists as for version (see upon call)
#   # for the moment checks are temporary
#   if (get_version_num(target_version) != 10)
#     stop(target_version, ": unknown version !")
#
#   # Fixed internally form the moment
#   # to get from info mentioned in comments at function head
#   # for getting list of versions which updates are taken into account
#   # for the target_version.
#   compat_versions <- "V9.2"
#   #compat_versions <- get_version_num(compat_versions)
#
#   # TODO: add list of compatible versions
#   #ver_num <- get_version_num(stics_version)
#   if (!stics_version %in% compat_versions) stop("Files from the version ", stics_version,
#                                           " cannot be converted to the version ", target_version)
#
#   # TODO: change from_tag to "9_1-2" if 9.1 and 9.2 are compatible !
#   list(from_tag = "9_2", to_tag = "10_0")
#
#
# }


# filter_usms <- function() {
#
#
# }




