#' @title Get the "formalisme" node "@nom" attribute value of a
#' (or a list of) parameter(s) in an xmlDocument
#' @param xml_doc an xmlDocument object (created from any xml file)
#' @param param_name a parameter name or a vector of
#'
#'
#' @return a formalism name or a list of
#'
#' @examples
#' \dontrun{
#'
#' xml_sta <- system.file(paste0("extdata/xml/examples/V9.1/file_sta.xml"),
#' package = "SticsRFiles")
#'
#' sta_doc <- SticsRFiles:::xmldocument(xml_sta)
#'
#' par_form <- SticsRFiles:::get_param_formalism(sta_doc,"zr")
#'
#' par_form_list <- SticsRFiles:::get_param_formalism(sta_doc,c("zr","altistation"))
#'
#' }
#'
#'
get_param_formalism <- function(xml_doc,param_name) {


  # recursive call for a parameter name list
  if (length(param_name) > 1) {
    form_list <- lapply(param_name, function(x) get_param_formalism(xml_doc,x))
    names(form_list) <- param_name
    return(form_list)
  }


  # Checking if any "formalisme" nodes in xml_doc
  form <- getAttrsValues(xml_doc,"//formalisme","nom")

  # exiting if no formalisms
  if (base::is.null(form)) {
    warning("Not any \"formalisme\" element in xml document !")
    return(NULL)
  }


  # case : param name as value of @nom attribute
  x_path <- paste0("//*[@nom=\"",param_name,"\"]/ancestor::formalisme")

  values <- getAttrsValues(xml_doc,x_path,"nom")

  if (! base::is.null(values)) {
    return(as.vector(values))
  }

  # case : param name as value of @nomParam attribute
  x_path <- paste0("//*[@nomParam=\"",param_name,"\"]/ancestor::formalisme")
  values <- getAttrsValues(xml_doc,x_path,"nom")

  if (! base::is.null(values)) {
    return(as.vector(values))
  }


  # TODO : other cases

  return(values)

}
