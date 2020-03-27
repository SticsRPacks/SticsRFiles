#' @title Generate from a template a Stics usms xmlDocument
#'
#' @param xml_doc an optional xmlDocument object (created from an usms.xml file)
#' @param usms_nb number of usms (usm nodes) to create in the xmlDocument (optional)
#' @param usms_param a table (df, tibble) containing parameters to use (optional)
#' @param stics_version the stics files version to use (optional, default to last). Only used if xml_doc = NULL.
#'
#' @return an invisible xmlDocument object
#'
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' xl_path <- "inputs_stics_example.xlsx"
#' download_usm_xl(xl_name = xl_path)
#' usms_param_df <- read_excel(xl_path, sheet = "USMs")
#' usms_doc <- SticsRFiles:::gen_usms_doc(usms_param = usms_param_df)
#' }
#'
#' @keywords internal
#'

gen_usms_doc <- function(xml_doc = NULL,
                         usms_nb = NULL,
                         usms_param = NULL,
                         stics_version ="last") {


  # replacement of NA values with "null"
  if (! base::is.null(usms_param)) {
    usms_param[is.na(usms_param)] <- "null"
  }

  out_doc_object <- gen_xml_doc(doc_type = "usms",
                                xml_doc,
                                nodes_nb = usms_nb,
                                nodes_param = usms_param,
                                stics_version = stics_version)

  return(invisible(out_doc_object))

}
