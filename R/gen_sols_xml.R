#' @title Generate from a template a Stics sols xml file
#' @param sols_out_file file path of the output sols xml file
#' @param sols_nb number of soils to create
#' @param sols_param a table (df, tibble) containing parameters to use
#' @param sols_in_file file path for an input sols xml file
#' @param stics_version the stics files version to use
#'
#' @return an invisible xmlDocument object
#'
#' @examples
#' \dontrun{
#' copy_mailing_example(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' sols_param_df <- read_excel(xl_path, sheet = "Soils")
#' gen_sols_xml(sols_out_file = file.path("/path/to/dest/dir","sols.xml"),
#' sols_param = sols_param_df)
#' }
#'
#' @export
#'
#'
#'
gen_sols_xml <- function(sols_out_file,
                          sols_nb = NULL,sols_param = NULL,
                          sols_in_file = NULL,
                          stics_version ="last") {

  xml_doc <- NULL

  if (! base::is.null(sols_in_file) ) {
    xml_doc <- xmldocument(sols_in_file)
  }

  xml_doc <- gen_sols_doc(xml_doc = xml_doc,
                          sols_nb = sols_nb,
                          sols_param = sols_param,
                          stics_version = stics_version)

  # checking if out dir exists
  out_path <- dirname(sols_out_file)
  if ( ! dir.exists(out_path) ) {
    stop(paste("The directory does not exist",out_path))
  }

  saveXmlDoc(xml_doc, sols_out_file)

  return(invisible(xml_doc))

}
