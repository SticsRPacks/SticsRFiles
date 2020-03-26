#' @title Generate from a template a Stics sols xmlDocument
#' @param xml_doc an xmlDocument object (created from a sols.xml file)
#'
#' @param sols_nb number of soils to create
#' @param sols_param a table (df, tibble) containing parameters to use
#' @param stics_version the stics files version to use
#'
#' @return an xmlDocument object
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' download_usm_xl(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' sols_param_df <- read_excel(xl_path, sheet = "Soils")
#' soil_doc <- SticsRFiles:::gen_sols_doc(sols_param = sols_param_df)
#' }
#'
#'
#' @keywords internal
#'
gen_sols_doc <- function(xml_doc = NULL,
                         sols_nb = NULL,
                         sols_param = NULL,
                         stics_version ="last") {


  out_doc_object <- gen_xml_doc(doc_type = "sols",
                                xml_doc = xml_doc,
                                nodes_nb = sols_nb,
                                nodes_param = sols_param,
                                stics_version = stics_version)

  return(out_doc_object)

}
