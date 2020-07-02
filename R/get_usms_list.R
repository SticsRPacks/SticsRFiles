#' @title Getting usms names list for an usms.xml file
#'
#' @description Extracting a usm names list from an usms.xml file
#' @param usm_path usm file path (e.g. "usms.xml")
#' @param name A vector of usm name(s) or partial name(s), optional
#'
#' @return A vector of usm names
#'
#' @examples
#' path = get_examples_path( file_type = "xml")
#'
#' usms_list <- get_usms_list(usm_path = file.path(path, "usms.xml"))
#'
#' usms_list <- get_usms_list(usm_path = file.path(path, "usms.xml"), name = c("usm1", "usm2"))
#'
#' @export
#'
get_usms_list <- function(usm_path, name = NULL){

  # TODO: add select key: i.e. get all usms names
  # with the same soil, plant 1,...

  # Detecting file type
  if ( !is_usms_xml(usm_path) ) {
    stop("The file must be a usm (usms) file")
  }

  return(find_usms_soils_names(file_path = usm_path, xml_name = "usm", name = name))

}


find_usms_soils_names <- function(file_path, xml_name, name = NULL) {

  # Getting names usms/soils list
  names_list <- get_param_xml(file_path, xml_name)[[1]][[xml_name]]

  # Not any names, (may be useless bcause xml file already checked!)
  if(is.null(names_list)) return(names_list)

  # Filter names list with partial match
  if (base::is.null(name)) return(names_list)

  # Filter usms/soils list with partial match
  return(find_names(names_list, name))
}


find_names <- function(names_list, name) {

  names_idx <- unlist(lapply(name, function(x) grep(pattern = x, x = names_list)))

  if (!length(names_idx)) return(NULL)

  names_list <- names_list[names_idx]

  return(names_list)

}
