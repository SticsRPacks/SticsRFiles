#' @title Get a set of a (or a list of) Stics xml parameter(s) values from a request
#' @title in an xmlDocument or a list of
#' @title Getting parameter value for different kinds of parameters
#'
#' @description Extracting parameter value from an xml document object
#'
#' @param xml_doc_object an xmlDocument object
#' @param param_name parameter name or a vector of names
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ids elements indices (optional)
#' @param show_xpath Print the xpath
#'
#' @return a numeric vector values of parameter or a list of
#'
#' @export
#'

get_param_value <- function(xml_doc_object,param_name,
                            parent_name= NULL, parent_sel_attr = NULL,
                            ids=NULL, show_xpath =FALSE){


  # TODO :
  # 1 - set default param_name value to NULL and calculate le param_name vector calling
  # the get_param_names function !
  # 2 - add the case : if param_name is a list of vectors of parameters names ?


  # Getting param values for sams parameters for the xml documents list
  if ( is.list(xml_doc_object) ) {
    values <- lapply(xml_doc_object,function(x) get_param_value(x,
                                                                param_name,
                                                                parent_name,
                                                                parent_sel_attr,
                                                                show_xpath = show_xpath))
    return(values)
  }

  # recursive call for a parameter name list
  if (length(param_name) > 1) {
    out <- lapply(param_name, function(x) get_param_value(xml_doc_object,x,
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


  param_type <- get_param_type(xml_doc_object,
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
           value=getValues(xml_doc_object,xpath,ids)
         },
         attr= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           value=getAttrsValues(xml_doc_object,xpath,param_type$attr,ids)
         },
         attrname= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           value=getAttrsValues(xml_doc_object,xpath,param_type$attr,ids)
         },
         param= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         option= {
           value=getAttrsValues(xml_doc_object,xpath,"choix",ids)
         },
         table= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         table2= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         node_param= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         choix_param= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         node_node= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         node_option= {
           value=getAttrsValues(xml_doc_object,xpath,"choix",ids)
         },
         form_option= {
           value=getAttrsValues(xml_doc_object,xpath,"choix",ids)
         },
         node_table= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         node_attr= {
           value=getAttrsValues(xml_doc_object,xpath,"nom",ids)
         },
         attr_attr= {
           value=getValues(xml_doc_object,xpath,ids)
         },
         attr_attr2= {
           value=getAttrsValues(xml_doc_object,xpath, param_name,ids)
         },
         choix_attr= {
           value=getAttrsValues(xml_doc_object,xpath,param_name,ids)
         }

         # TODO : add other cases for tables in ini, soil, and other specific parameters
  )


  # Converting value to numeric if not any character in it
  # numbers may contain scientific notation e+ e-, decimal, and space
  #nb_num <- length(grep("[^- |0-9|.|e|+]",value, invert = T))
  num_value <- as.numeric(value)
  is_number <- suppressWarnings(!is.na(num_value))
  #if ( nb_num == length(value) )  {
  if ( all(is_number) )  {
    value <- as.numeric(num_value)
  }

  # TODO: see if finally useless, checks done in getValues ?
  # if (! base::is.null(ids) ) {
  #   # check ids
  #   if ( max(ids) > param_type$length ) {
  #     stop("Subscript out of range, check ids !")
  #   }
  #   #value=value[ids]
  # }


  return(as.vector(value))
}
