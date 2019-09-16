#' @title Generate from a template a Stics sols xmlDocument
#' @param xml_doc_object an xmlDocument object (created from a sols.xml file)
#'
#' @param sols_nb number of soils to create
#' @param sols_param a table (df, tibble) containing parameters to use
#' @param stics_version the stics files version to use
#'
#' @return an xmlDocument object
#'
#' @export
#'
gen_sols_doc <- function(xml_doc_object = NULL,
                         sols_nb = NULL,
                         sols_param = NULL,
                         stics_version ="last") {


  out_doc_object <- gen_xml_doc(doc_type = "sols",
                                xml_doc_object,
                                nodes_nb = sols_nb,
                                nodes_param = sols_param,
                                stics_version = stics_version)

  return(out_doc_object)

}
