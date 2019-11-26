get_options_names <- function(xml_file_path){
  #' @title Getting options parameter names from an xml parameter file
  #'
  #' @description Extracting options parameter name from an xml file data
  #'
  #' @param xml_file_path path of xml parameter file
  #'
  #' @return A vector of strings of options parameter names
  #'
  #' @examples
  #' \dontrun{
  #' get_options_names(path/to/xml/file)
  #' }
  #'
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2018-01-11 17:46:03 +0100 (jeu. 11 janv. 2018) $
  #  $Author: plecharpent $
  #  $Revision: 1113 $
  # ----------------------------------------------------------------------

  xml_param=xmldocument(xml_file_path)
  xpath=paste0('//option')
  param_names=getAttrsValues(xml_param,xpath,"nomParam")
  return(param_names)
}
