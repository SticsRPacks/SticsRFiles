#' @title Get a set of a (or a list of) Stics xml parameter(s) values from a request
#' @title in an xmlDocument or a list of
#' @title Getting parameter value for different kinds of parameters
#'
#' @description Extracting parameter value from an xml document object
#'
#' @param xml_doc an xmlDocument object
#' @param param_name parameter name or a vector of names
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ids elements indices (optional)
#' @param show_xpath Print the xpath
#'
#' @return a numeric vector values of parameter or a list of
#'
#' @examples
#'
#' \dontrun{
#' xml_path = system.file("extdata/xml/examples/V9.0/sols.xml", package = "SticsRFiles")
#' sols_doc <- SticsRFiles:::xmldocument(xml_path)
#' SticsRFiles:::get_param_value(sols_doc, "argi")
#' SticsRFiles:::get_param_value(sols_doc, c("argi", "norg"))
#'
#' SticsRFiles:::get_param_value(sols_doc, "argi",
#' parent_name = "sol", parent_sel_attr = "solcanne"))
#'
#' SticsRFiles:::get_param_value(sols_doc, c("argi", "norg"),
#' parent_name = "sol", parent_sel_attr = c("solcanne", "solbanane"))
#'
#' SticsRFiles:::get_param_value(list(sols_doc, sols_doc), c("argi","norg"),
#' parent_name = "sol", parent_sel_attr = c("solcanne","solbanane"))
#'
#' }
#'
#' @keywords internal
#'

get_param_value <- function(xml_doc,
                            param_name,
                            parent_name= NULL,
                            parent_sel_attr = NULL,
                            ids = NULL,
                            show_xpath =FALSE){


  # TODO :
  # 1 - set default param_name value to NULL and calculate le param_name vector calling
  # the get_param_names function !
  # 2 - add the case : if param_name is a list of vectors of parameters names ?


  # Getting param values for sams parameters for the xml documents list
  if ( is.list(xml_doc) ) {
    values <- lapply(xml_doc, function(x) get_param_value(x,
                                                         param_name,
                                                         parent_name,
                                                         parent_sel_attr,
                                                         ids = ids,
                                                         show_xpath = show_xpath))
    return(values)
  }

  # recursive call for a parameter name list
  if (length(param_name) > 1) {
    out <- lapply(param_name, function(x) get_param_value(xml_doc,
                                                          x,
                                                          parent_name,
                                                          parent_sel_attr,
                                                          ids = ids,
                                                          show_xpath = show_xpath))
    names(out) <- param_name
    return(out)
  }

  param_types=get_param_type()

  if (is.numeric(parent_name)) {
    ids <- parent_name
    parent_name <- NULL
  }

  if (is.numeric(parent_sel_attr)) {
    ids <- parent_sel_attr
    parent_sel_attr <- NULL
  }


  param_type <- get_param_type(xml_doc,
                               param_name, parent_name, parent_sel_attr, ids)
  type <- param_type$type
  xpath <- param_type$xpath

  value = NULL

  if ( show_xpath ) {
    print(xpath)
  }

  if ( base::is.null(xpath) ) {
    #stop("Unknown parameter name !")
    return(value)
  }



  # TODO: see if could be simplified with a default case !
  switch(type,
         nodename= {
           value=getValues(xml_doc,xpath,ids)
         },
         attr= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           value=getAttrsValues(xml_doc,xpath,param_type$attr,ids)
         },
         attrname= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           value=getAttrsValues(xml_doc,xpath,param_type$attr,ids)
         },
         param= {
           value=getValues(xml_doc,xpath,ids)
         },
         option= {
           value=getAttrsValues(xml_doc,xpath,"choix",ids)
         },
         table= {
           value=getValues(xml_doc,xpath,ids)
         },
         table2= {
           value=getValues(xml_doc,xpath,ids)
         },
         node_param= {
           value=getValues(xml_doc,xpath,ids)
         },
         choix_param= {
           value=getValues(xml_doc,xpath,ids)
         },
         node_node= {
           value=getValues(xml_doc,xpath,ids)
         },
         node_option= {
           value=getAttrsValues(xml_doc,xpath,"choix",ids)
         },
         form_option= {
           value=getAttrsValues(xml_doc,xpath,"choix",ids)
         },
         node_table= {
           value=getValues(xml_doc,xpath,ids)
         },
         node_attr= {
           value=getAttrsValues(xml_doc,xpath,"nom",ids)
         },
         attr_attr= {
           value=getValues(xml_doc,xpath,ids)
         },
         attr_attr2= {
           value=getAttrsValues(xml_doc,xpath, param_name,ids)
         },
         choix_attr= {
           value=getAttrsValues(xml_doc,xpath,param_name,ids)
         }

         # TODO : add other cases for tables in ini, soil, and other specific parameters
  )


  # Converting value to numeric if not any character in it
  # numbers may contain scientific notation e+ e-, decimal, and space
  #nb_num <- length(grep("[^- |0-9|.|e|+]",value, invert = T))
  num_value <- suppressWarnings(as.numeric(value))
  is_number <- suppressWarnings(!is.na(num_value))
  #if ( nb_num == length(value) )  {
  if ( all(is_number) )  {
    #value <- suppressWarnings(as.numeric(num_value))
    value <- num_value
  }

  # TODO: see if finally useless, checks done in getValues ?
  # if (! base::is.null(ids) ) {
  #   # check ids
  #   if ( max(ids) > param_type$length ) {
  #     stop("Subscript out of range, check ids !")
  #   }
  #   #value=value[ids]
  # }


  value <- suppressWarnings(as.vector(value))

  return(value)
}
