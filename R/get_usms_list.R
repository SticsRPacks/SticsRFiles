#' @title Getting usms names list for an usms.xml file
#'
#' @description Extracting a usm names list from an usms.xml file
#'
#' @param file Path (including name) of the USM xml file
#' @param usm Vector of USM names (or partial names).
#' Optional, if not provided, the function returns the names of all the USMs
#' included in the given file.
#'
#' @return A vector of usm names
#'
#' @examples
#' path <- get_examples_path(file_type = "xml")
#'
#' usms_list <- get_usms_list(file = file.path(path, "usms.xml"))
#'
#' usms_list <- get_usms_list(
#'   file = file.path(path, "usms.xml"),
#'   usm = c("usm1", "usm2")
#' )
#'
#' @export
#'
get_usms_list <- function(
  file,
  usm = NULL
) {
  xml_doc <- xmldocument(file)

  # Detecting file type
  if (!is_stics_usms(xml_doc)) {
    stop("The file does not exist or is not a usms file: ", file)
  }

  return(
    find_usms_soils_names(
      xml_doc = xml_doc,
      xml_name = "usm",
      name = usm
    )
  )
}


find_usms_soils_names <- function(xml_doc, xml_name, name = NULL) {
  # Getting names usms/soils list
  names_list <- unique(
    unlist(
      lapply(
        XML::getNodeSet(doc = xml_doc@content, path = paste0("//", xml_name)),
        function(x) {
          switch(xml_name,
            nomsol = XML::xmlValue(x),
            # default case handling xml_namevalues : sol, usm
            XML::xmlGetAttr(x, "nom")
          )
        }
      )
    )
  )

  # Not any names, (may be useless bcause xml file already checked!)
  if (is.null(names_list)) {
    return(names_list)
  }

  # Filter names list with partial match
  if (base::is.null(name)) {
    return(names_list)
  }

  # Filter usms/soils list with partial match
  return(find_names(names_list, name))
}
