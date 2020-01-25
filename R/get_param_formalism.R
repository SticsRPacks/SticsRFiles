#' @title Get the "formalisme" node "@nom" attribute value of a
#' (or a list of) parameter(s) in an xmlDocument, of a vector of
#' @param xml_doc an xmlDocument object (created from any xml file),
#' or a vector of
#' @param name Optional. A parameter name or a vector of. If not given,
#' all parameters names are extracted.
#'
#'
#' @return a list parameters formalism name
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
#' @keywords internal
#'
#'
get_param_formalism <- function(xml_doc, name = NULL, form_only = FALSE) {

  # For multiple documents
  if ( length(xml_doc) > 1 ) {

    names <- lapply(xml_doc,
                    function(x) get_param_formalism(xml_doc = x,
                                                    name = name,
                                                    form_only = form_only))


    return(names)
  }

  # If no parameter name is given
  if (base::is.null(name)) name <- get_param_names(xml_doc)

  # recursive call for a parameter name list
  if (length(name) > 1) {
    form_list <- lapply(name, function(x) get_param_formalism(xml_doc,
                                                              x,
                                                              form_only = form_only))

    names(form_list) <- name

    if (form_only)  form_list <- unique(unlist(form_list, use.names = FALSE))

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
  x_path <- paste0("//*[@nom=\"",name,"\"]/ancestor::formalisme")

  values <- getAttrsValues(xml_doc,x_path,"nom")

  if (! base::is.null(values)) {
    return(as.vector(values))
  }

  # case : param name as value of @nomParam attribute
  x_path <- paste0("//*[@nomParam=\"",name,"\"]/ancestor::formalisme")
  values <- getAttrsValues(xml_doc,x_path,"nom")

  if (! base::is.null(values)) {
    return(as.vector(values))
  }


  # TODO : other cases

  return(values)

}
