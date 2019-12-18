#' @title Setting parameter value for different kinds of parameters
#'
#' @description Setting parameter value in a xmlDocument object
#'
#' @param xml_doc_object an xmlDocument object
#' @param param_name parameter name
#' @param param_value parameter value
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ids elements indices (optional)
#' @param show_xpath Print the xpath
#'
#@export
set_param_value <- function(xml_doc_object,param_name,param_value,
                            parent_name= NULL,
                            parent_sel_attr = NULL, ids=NULL,
                            show_xpath =FALSE){

  # for managing vector values
  param_value <- as.list(param_value)

  # calling the for several parameters
  nb_param <- length(param_name)
  if ( nb_param > 1) {
    for (p in 1:nb_param) {
      set_param_value(xml_doc_object,param_name = param_name[p], param_value = param_value[[p]])
    }
    return(invisible())
  }

  # back to scalar or vector
  param_value <- unlist(param_value)

  param_types=get_param_type()

  if ( is.numeric(parent_name) ) {
    ids = parent_name
    parent_name = NULL
  }

  if (is.numeric(parent_sel_attr)) {
    ids <- parent_sel_attr
    parent_sel_attr <- NULL
  }

  param_type <- get_param_type(xml_doc_object, param_name,
                               parent_name,parent_sel_attr)#, ids)
  type <- param_type$type
  xpath <- param_type$xpath

  if ( show_xpath ) {
    print(xpath)
  }

  if (! is.element(type,param_types) ) {
    stop("Type error !")
  }

  values_nb <- length(param_value)
  nodes_nb <- length(getNodeS(xml_doc_object,xpath))
  # checking dimensions between nodes ids and nodes number for xpath
  if ( base::is.null(ids) && values_nb > 1 && !values_nb == nodes_nb ) {
    stop("Replacement values are not consistent with parameters number !")
  }

  # TODO: see if could be simplified with a default case !
  switch(type,
         nodename= {
           setValues(xml_doc_object,xpath,param_value, ids)
         },
         attr= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           setAttrValues(xml_doc_object,xpath,"nom",param_value, ids)
         },
         attrname= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           setAttrValues(xml_doc_object,xpath,param_name,param_value, ids)
         },
         param= {
           #xpath=paste0('//param[@nom="',param_name,'"]')

           setValues(xml_doc_object,xpath,param_value, ids)
         },
         option= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           setAttrValues(xml_doc_object,xpath,"choix",param_value, ids)
         },
         table= {
           #xpath=paste0('//param[@nom="',param_name,'"]')

           # check number if values
           if ( length(param_value) > param_type$length ) {
             stop("Too many values to set !")
           }
           setValues(xml_doc_object,xpath,param_value, ids)
         },
         table2= {

           if ( length(param_value) > param_type$length ) {
             stop("Too many values to set !")
           }
           setValues(xml_doc_object,xpath,param_value, ids)
         },
         node_param= {
           setValues(xml_doc_object,xpath,param_value,ids)
         },
         choix_param= {
           setAttrValues(xml_doc_object,xpath,"choix",param_value,ids)
         },
         node_node= {
           setValues(xml_doc_object,xpath,param_value,ids)
         },

         node_option= {
           setAttrValues(xml_doc_object,xpath,"choix",param_value,ids)
         },

         form_option= {
           setAttrValues(xml_doc_object,xpath,"choix",param_value,ids)
         },
         node_table= {
           setValues(xml_doc_object,xpath,param_value,ids)
         },
         node_attr= {
           setAttrValues(xml_doc_object,xpath,"nom",param_value,ids)
         },
         attr_attr= {
           setValues(xml_doc_object,xpath,param_value,ids)
         },
         attr_attr2= {
           value=setAttrValues(xml_doc_object,xpath, param_name,param_value,ids)
         },
         choix_attr= {
           value=setAttrValues(xml_doc_object,xpath,param_name,param_value,ids)
         }


  )


  #return(xml_doc_object)
  return(invisible())
}
