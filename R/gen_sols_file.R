#' @title Generate from a template a Stics sols xml file
#' @param sols_out_file file path of the output sols xml file
#'
#' @param sols_nb number of soils to create
#' @param sols_param a table (df, tibble) containing parameters to use
#' @param sols_in_file file path for an input sols xml file
#' @param stics_version the stics files version to use
#'
#' @return an invisible xmlDocument object
#'
#' @export
#'
#'
#'
gen_sols_file <- function(sols_out_file,
                          sols_nb = NULL,sols_param = NULL,
                          sols_in_file = NULL,
                          stics_version ="last") {

  xml_doc <- NULL

  if (! is.null(sols_in_file) ) {
    xml_doc <- xmldocument(sols_in_file)
  }

  xml_doc <- gen_sols_doc(xml_doc = xml_doc,
                          sols_nb = sols_nb,
                          sols_param = sols_param,
                          stics_version = stics_version)


  saveXmlDoc(xml_doc, sols_out_file)

  return(invisible(xml_doc))

}
