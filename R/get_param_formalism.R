#' @title Get the "formalisme" node "@nom" attribute value of a (or a list of) parameter(s) in an xmlDocument
#' @param xml_document an xmlDocument object (created from any xml file)
#' @param param_name parameter name or vector of names
#'
#' @return a formalism name or a list of
#' @export
#'
get_param_formalism <- function(xml_document,param_name) {


  # recursive call for a parameter name list
  if (length(param_name) > 1) {
    form_list <- lapply(param_name, function(x) get_param_formalism(xml_document,x))
    names(form_list) <- param_name
    return(form_list)
  }


  # case : param name as value of @nom attribute
  x_path <- paste0("//*[@nom=\"",param_name,"\"]/ancestor::formalisme")

  values <- getAttrsValues(xml_document,x_path,"nom")

  if (! base::is.null(values)) {
    return(as.vector(values))
  }

  # case : param name as value of @nomParam attribute
  x_path <- paste0("//*[@nomParam=\"",param_name,"\"]/ancestor::formalisme")
  values <- getAttrsValues(xml_document,x_path,"nom")

  if (! base::is.null(values)) {
    return(as.vector(values))
  }




  # TODO : other cases

}
