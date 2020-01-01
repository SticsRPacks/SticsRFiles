#' @title Getting options parameter choices from an xml parameter file
#'
#' @description Extracting options choices values from an xml file data
#'
#' @param xml_file_path path of xml parameter file
#' @param options_names options names to take into account (optional)
#'
#' @return A list of strings of options choice values
#'
#' @examples
#' \dontrun{
#'
#' xml_path= file.path(system.file(package="SticsRFiles","extdata/xml/examples/V9.0/baresoil_plt.xml"))
#'
#' # For getting all options choices
#' SticsRFiles:::get_options_choices(xml_path)
#'
#' # For getting one option choices or more
#' SticsRFiles:::get_options_choices(xml_path,"codetemp")
#' SticsRFiles:::get_options_choices(xml_path,c("codegdh", "codetemp"))
#' }
#'
#' @keywords internal
#'
get_options_choices <- function(xml_file_path, options_names=NULL){


  # If no options_names given, taking the full list
  if (base::is.null(options_names)) {
    names_vec=get_options_names(xml_file_path)
  } else {
    names_vec = options_names
  }

  options_choices = vector("list",length(names_vec))

  # Loading xml file
  xml_param=xmldocument(xml_file_path)

  # Getting for each option, choices name and code
  for (n in 1:length(names_vec)) {
    xpath=paste0('//option[@nomParam="',names_vec[n],'"]/choix')
    options_choices[[n]]=getAttrsValues(xml_param,xpath,"code")
    names(options_choices[[n]]) <- getAttrsValues(xml_param,xpath,"nom")
  }

  # naming options_choices list with options names
  names(options_choices) <- names_vec

  return(options_choices)
}
