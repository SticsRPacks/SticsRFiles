#' @title Getting options parameter choices from an xml parameter file
#'
#' @description Extracting options choices values from an xml file data
#'
#' @param xml_file_path path of xml parameter file
#' @param options_names options names to take into account
#'
#' @return A list of strings of options choice values
#'
#' @examples
#' \dontrun{
#' get_option_choices(path/to/xml/file)
#' }
#'
#'
get_options_choices <- function(xml_file_path, options_names=NULL){


  if (base::is.null(options_names)) {
    names_vec=get_options_names(xml_file_path)
  } else {
    names_vec = options_names
  }

  options_choices = vector("list",length(names_vec))

  xml_param=xmldocument(xml_file_path)
  for (n in 1:length(names_vec)) {
    xpath=paste0('//option[@nomParam="',names_vec[n],'"]/choix')
    options_choices[[n]]=getAttrsValues(xml_param,xpath,"code")
  }


  return(options_choices)
}
