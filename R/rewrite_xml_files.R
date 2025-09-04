#' Rewriting a usms.xml file for a given usms list
#'
#' @param usms_file usms.xml file path
#' @param out_dir output directory path
#' @param usm vector of usm names (optional)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' rewrite_usms_file(/path/to/usms/file,
#'                   /path/to/output/dir,
#'                   c("SugarCane", "potato"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
rewrite_usms_file <- function(usms_file, out_dir, usm = NULL) {
  # if usm is NULL, all usms are kept
  # copying the usms file to out_dir
  if (is.null(usm)) {
    file.copy(from = usms_file, to = out_dir)
    return(invisible(NULL))
  }

  usms_doc <- xmldocument(usms_file)

  all_usms <- get_usms_list(file = usms_file)

  unknown_usms <- setdiff(usm, all_usms)

  # warning if unknown usm
  if (length(unknown_usms) > 0) {
    warning("Unknown usm(s) name(s): ", paste(unknown_usms, collapse = ";"))
  }

  # setting the usms filter
  usms_to_remove <- all_usms[!(all_usms %in% usm)]

  if (length(usms_to_remove) == length(all_usms))
    stop("Not any remaining usm to write in usms.xml file")

  if (length(usms_to_remove) == 0) {
    file.copy(from = usms_file, to = out_dir)
    return(invisible(NULL))
  }

  # removing useless usm nodes
  nodes_to_remove <- unlist(lapply(usms_to_remove, function(x) {
    xpath = paste0('//usm[@nom="', x, '"]')
    XML::getNodeSet(usms_doc@content, xpath)
  }))

  XML::removeNodes(nodes_to_remove)

  # Writing the file
  write(XML::saveXML(usms_doc@content), file.path(out_dir, "usms.xml"))

  # freeing memory
  delete(usms_doc)
}


#' Rewriting a sols.xml file for a given usms list
#'
#' @param usms_file usms.xml file path
#' @param sols_file sols.xml file path
#' @param out_dir output directory path
#' @param usm vector of usm names (optional)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' rewrite_sols_file(/path/to/usms/file,
#'                   /path/to/sols/file,
#'                   /path/to/output/dir,
#'                   c("SugarCane", "potato"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
rewrite_sols_file <- function(usms_file, sols_file, out_dir, usm = NULL) {
  # if usm is NULL, all usms are kept
  # copying the usms file to out_dir
  if (is.null(usm)) {
    file.copy(from = sols_file, to = out_dir)
    return(invisible(NULL))
  }

  sols_doc <- xmldocument(sols_file)

  all_usms <- get_usms_list(usms_file)
  all_sols <- get_soils_list(sols_file)

  unknown_usms <- setdiff(usm, all_usms)

  if (length(unknown_usms) > 0) {
    warning("Unknown usm(s) name(s): ", paste(unknown_usms, collapse = ";"))
  }

  # getting a list of unique soils used in the selected usms
  usms_sols <- unique(
    (get_param_xml(
      file = usms_file,
      param = "nomsol"
    )$usms.xml$nomsol)[all_usms %in% usm]
  )

  # getting the soils to remove
  sols_to_remove <- setdiff(all_sols, usms_sols)

  if (length(sols_to_remove) == length(all_sols))
    stop("Not any remaining sols to write in sols.xml file")

  if (length(sols_to_remove) == 0) {
    file.copy(from = sols_file, to = out_dir)
    return(invisible(NULL))
  }

  # removing useless sol nodes
  nodes_to_remove <- unlist(lapply(sols_to_remove, function(x) {
    xpath = paste0('//sol[@nom="', x, '"]')
    XML::getNodeSet(sols_doc@content, xpath)
  }))

  XML::removeNodes(nodes_to_remove)

  # Writing the file
  write(XML::saveXML(sols_doc@content), file.path(out_dir, "sols.xml"))

  # freeing memory
  delete(sols_doc)
}
