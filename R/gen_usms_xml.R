#' @title Generate from a template a Stics usms xml file
#' @param usms_out_file file name of the output usms xml file
#'
#' @param usms_nb number of usms to create
#' @param usms_param a table (df, tibble) containing parameters to use
#' @param usms_in_file file path for an input usms xml file
#' @param stics_version the stics files version to use
#'
#'
#' @return an invisible xmlDocument object
#'
#' @examples
#' \dontrun{
#' copy_mailing_example(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' usms_param_df <- read_excel(xl_path, sheet = "USMs")
#' gen_usms_xml(usms_out_file = file.path("/path/to/dest/dir","usms.xml"),
#' usms_param = usms_param_df)
#' }
#'
#' @export
#'

gen_usms_xml <- function(usms_out_file,
                          usms_nb = NULL, usms_param = NULL,
                          usms_in_file = NULL,
                          stics_version ="last") {


  xml_doc <- NULL

  if (! base::is.null(usms_in_file) ) {
    xml_doc <- xmldocument(usms_in_file)
  }

  vars <- c("flai_1", "fplt_2","ftec_2", "flai_2")
  vars_idx <- vars %in% names(usms_param)

  # replacing NA with "null", if any vars name in usms_param names
  if (! base::is.null(usms_param) && any(vars_idx)) {
    for (v in vars[vars_idx]) {
      usms_param[[v]][is.na(usms_param[[v]])] <- "null"
    }
  }

  xml_doc <- gen_usms_doc(xml_doc_object = xml_doc,
                          usms_nb = usms_nb,
                          usms_param = usms_param,
                          stics_version = stics_version)

  # checking if out dir exists
  out_path <- dirname(usms_out_file)
  if ( ! dir.exists(out_path) ) {
    stop(paste("The directory does not exist",out_path))
  }


  saveXmlDoc(xml_doc, usms_out_file)

  return(invisible(xml_doc))

}
