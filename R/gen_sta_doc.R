#' @title Generate  from a template or modify a Stics sta xmlDocument
#' @param xml_doc_object an xmlDocument object (created from an sta file)
#'
#' @param param_table a table (df, tibble) containing parameters to use
#' @param stics_version the stics files version to use
#'
#' @return an xmlDocument object or a list of
#'
#'
#' @examples
#' \dontrun{
#' copy_mailing_example(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' sta_param_df <- read_excel(xl_path, sheet = "Station")
#' sta_doc <- SticsRFiles:::gen_sta_doc(param_table = sta_param_df)
#' }
#'
#' @keywords internal
#'
gen_sta_doc <- function(xml_doc_object = NULL,
                        param_table = NULL,
                        stics_version ="last") {


  # check/get version
  stics_version <- get_xml_stics_version(stics_version = stics_version,xml_doc = xml_doc_object)

  # getting a default xml template
  if ( base::is.null(xml_doc_object) ) {
    xml_doc_object <- get_xml_base_doc("sta", stics_version = stics_version)
  }

  # # Nothing to do
  # if ( base::is.null(param_table) ) {
  #   return(xml_doc_object)
  # }



  # managing several doc generation based upon the lines number in param_table
  lines_nb <- dim(param_table)[1]
  if (lines_nb > 1) {

    xml_docs <- apply(param_table,1,
                      function(x) gen_sta_doc(xml_doc_object = cloneXmlDoc(xml_doc_object),
                                              param_table = as.data.frame(t(x)),
                                              stics_version = stics_version))
    return(xml_docs)
  }

  #print(stics_version)

  in_params <- names(param_table)
  for (p in in_params) {
    set_param_value(xml_doc_object,p, param_table[[p]])
  }


  return(xml_doc_object)

}
