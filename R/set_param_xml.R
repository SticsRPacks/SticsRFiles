#' @title Setting parameter values into xml files
#'
#' @description Setting parameter values for a parameter or a vector of and
#' with a parameters values vector
#'
#' @param xml_file an xml file path
#' @param param_names parameter names vector
#' (i.e.: option parameter name, parameter name,
#' other kinds in ini and some tec parameters not taken into account for the moment)
#' @param param_values parameter(s) values or a list of
#' @param out_path a directory path
#' @param out_file an xml file name
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ids elements indices (optional)
#' @param show_xpath Logical for displaying or not xpath value
#' @param overwrite Logical TRUE for overwriting the output file, FALSE otherwise (default)
#'
#' @return A logical value TRUE for operation success, FALSE otherwise
#'
#' @examples
#'
#' \dontrun{
#' xml_path = system.file("extdata/xml/examples/V9.1/sols.xml", package = "SticsRFiles")
#' file.copy(xml_path, getwd())
#' get_param_xml("sols.xml", "argi")
#'
#' # Setting all "argi" values to 50
#' set_param_xml("sols.xml", "argi", 50)
#'
#' get_param_xml("sols.xml", "argi")
#'
#' # Setting a specific value to "argi" for "solcanne" soil
#' set_param_xml("sols.xml", "argi",56,
#' parent_name = "sol", parent_sel_attr = "solcanne")
#'
#' get_param_xml("sols.xml", "argi",
#' parent_name = "sol", parent_sel_attr = "solcanne")
#'
#'
#' # Setting a specific values to 2 parameters "argi" and "norg" for "solcanne" soil
#' set_param_xml("sols.xml", c("argi", "norg"),list(100,150),
#' parent_name = "sol", parent_sel_attr = "solcanne")
#'
#' get_param_xml("sols.xml", c("argi", "norg"),
#' parent_name = "sol", parent_sel_attr = "solcanne")
#'
#' }
#'
#' @export
set_param_xml <- function(xml_file,
                          param_names,
                          param_values,
                          out_path = NULL,
                          out_file = NULL,
                          parent_name = NULL,
                          parent_sel_attr = NULL,
                          ids = NULL,
                          show_xpath = FALSE,
                          overwrite = FALSE){


  #  Setting output directory path or checking
  if (base::is.null(out_path)) {
    out_path <- base::dirname(xml_file)
    if (out_path == ".") out_path <- getwd()

  } else if ( !base::file.exists(out_path) ) {
    warning(paste("The directory does not exist:", out_path))
    return(FALSE)
  }


  # Setting output file path
  if (! base::is.null(out_file) ) {
    out_path <- base::file.path(out_path, out_file)
  } else {
    out_path <- base::file.path(out_path, base::basename(xml_file))
  }

  # Ckecking if file exists and overwriting right
  if ( base::file.exists(out_path) && !overwrite ) {
    warning(paste("The file already exists, set overwrite argument to TRUE or delete the file: ",
                  out_path))
    return(invisible(FALSE))
  }

  # For future version
  # TODO: multiple files and multiple params list and values ...ids ...?
  #xml_doc <- lapply(xml_file, xmldocument)
  xml_doc <- xmldocument(xml_file)

  # Setting parameters values in the xmlDoxument object
  set_param_value(xml_doc,
                  param_name = param_names,
                  param_value = param_values,
                  parent_name = parent_name,
                  parent_sel_attr = parent_sel_attr,
                  ids = ids,
                  show_xpath = show_xpath)


  # Saving
  saveXmlDoc(xml_doc, out_path)

  # Output status
  return(invisible(TRUE))


}
