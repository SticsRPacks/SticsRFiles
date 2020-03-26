#' @title Generate  from a template or modify a Stics usms xmlDocument
#' @param xml_doc an xmlDocument object (created from an ini file)
#'
#' @param usms_nb number of usms to create
#' @param usms_param a table (df, tibble) containing parameters to use
#' @param stics_version the stics files version to use
#'
#' @return an xmlDocument object
#'
#'
#' @examples
#' \dontrun{
#' #' library(readxl)
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

  return(out_doc_object)

}
