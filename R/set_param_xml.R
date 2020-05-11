#' @title Setting parameter values into xml files
#'
#' @description Setting parameter values for a parameter or a vector of and
#' with a parameters values vector
#'
#' @param xml_file an xml file path
#' @param param_name parameter names vector, i.e.: parameter name or option code
#' @param param_value A vector or a list of parameter(s) values (see details).
#' @param out_path a directory path (optional, default uses the folder of the path from xml_file)
#' @param out_file an xml file name (optional, default to xml_file)
#' @param select node name or attribute name to use for selection (optional, default to no selection)
#' @param value value used for select (optional)
#' @param overwrite Logical TRUE for overwriting the output file, FALSE otherwise (default)
#' @param... Pass further arguments to `set_param_value()`.
#'
#' @return A logical value TRUE for operation success, FALSE otherwise
#'
#' @details It is possible to give several values for a parameter by passing a vector of values. For example
#' for two parameters with two values each: param_value= list(c(1,2), c(2.3,4.5))
#'
#' @examples
#'
#' \dontrun{
#' xml_path = file.path(get_examples_path( file_type = "xml"),"sols.xml")
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
#' select = "sol", value = "solcanne")
#'
#' get_param_xml("sols.xml", "argi",
#' select = "sol", value = "solcanne")
#'
#'
#' # Setting a specific values to 2 parameters "argi" and "norg" for "solcanne" soil
#' set_param_xml("sols.xml", c("argi", "norg"),list(100,150),
#' select = "sol", value = "solcanne")
#'
#' get_param_xml("sols.xml", c("argi", "norg"),
#' select = "sol", value = "solcanne")
#'
#' }
#'
#' @export
set_param_xml <- function(xml_file,
                          param_name,
                          param_value,
                          out_path = NULL,
                          out_file = NULL,
                          select = NULL,
                          value = NULL,
                          overwrite = FALSE,
                          ...){

  # ... argument for passing : ids, show_xpath to get_param_value




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
                  param_name = param_name,
                  param_value = param_value,
                  parent_name = select,
                  parent_sel_attr = value,
                  ...)


  # Saving
  saveXmlDoc(xml_doc, out_path)

  # Output status
  return(invisible(TRUE))


}
