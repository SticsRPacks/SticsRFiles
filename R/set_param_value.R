#' @title Setting parameter value for different kinds of parameters
#'
#' @description Setting parameter value in a xmlDocument object
#'
#' @param xml_doc an xmlDocument object
#' @param param_name parameter name
#' @param param_value parameter value
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ids elements indices (optional)
#' @param show_xpath Print the xpath
#'
#' @examples
#' \dontrun{
#' xml_path = system.file("extdata/xml/examples/V9.0/sols.xml", package = "SticsRFiles")
#' sols_doc <- SticsRFiles:::xmldocument(xml_path)
#' SticsRFiles:::get_param_value(sols_doc, "argi")
#'
#'  [1] 30.2 21.0 27.0 39.0  1.0 12.2 70.0 22.0  9.9 10.2 10.2 17.0 23.1 22.0 27.0 30.7  0.1
#' [18] 27.3 25.0 10.2 25.0 28.6 36.0 29.0 10.2 21.2 22.2 13.0 17.0 15.0 26.0 28.2 20.0
#'
#' # setting all argi parameters with the same value
#' SticsRFiles:::set_param_value(sols_doc, "argi", 15)
#' SticsRFiles:::get_param_value(sols_doc, "argi")
#'
#'  [1] 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15
#' [29] 15 15 15 15 15
#'
#' # setting specific values for some soils
#' SticsRFiles:::set_param_value(sols_doc, "argi", c(30,35),
#' parent_name = "sol", parent_sel_attr = c("solcanne","solbanane"))
#' SticsRFiles:::get_param_value(sols_doc, "argi")
#'
#'  [1] 30 15 15 15 15 15 35 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15
#' [29] 15 15 15 15 15
#'
#' }
#'
#' @keywords internal
#'
set_param_value <- function(xml_doc,param_name,param_value,
                            parent_name= NULL,
                            parent_sel_attr = NULL, ids=NULL,
                            show_xpath =FALSE){

  # for managing vector values
  param_value <- as.list(param_value)

  # calling the for several parameters
  nb_param <- length(param_name)
  if ( nb_param > 1) {
    for (p in 1:nb_param) {
      set_param_value(xml_doc,param_name = param_name[p], param_value = param_value[[p]])
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

  param_type <- get_param_type(xml_doc, param_name,
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
  nodes_nb <- length(getNodeS(xml_doc,xpath))
  # checking dimensions between nodes ids and nodes number for xpath
  if ( base::is.null(ids) && values_nb > 1 && !values_nb == nodes_nb ) {
    stop("Replacement values are not consistent with parameters number !")
  }

  # TODO: see if could be simplified with a default case !
  switch(type,
         nodename= {
           setValues(xml_doc,xpath,param_value, ids)
         },
         attr= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           setAttrValues(xml_doc,xpath,"nom",param_value, ids)
         },
         attrname= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           setAttrValues(xml_doc,xpath,param_name,param_value, ids)
         },
         param= {
           #xpath=paste0('//param[@nom="',param_name,'"]')

           setValues(xml_doc,xpath,param_value, ids)
         },
         option= {
           #xpath=paste0('//option[@nomParam="',param_name,'"]')
           setAttrValues(xml_doc,xpath,"choix",param_value, ids)
         },
         table= {
           #xpath=paste0('//param[@nom="',param_name,'"]')

           # check number if values
           if ( length(param_value) > param_type$length ) {
             stop("Too many values to set !")
           }
           setValues(xml_doc,xpath,param_value, ids)
         },
         table2= {

           if ( length(param_value) > param_type$length ) {
             stop("Too many values to set !")
           }
           setValues(xml_doc,xpath,param_value, ids)
         },
         node_param= {
           setValues(xml_doc,xpath,param_value,ids)
         },
         choix_param= {
           setAttrValues(xml_doc,xpath,"choix",param_value,ids)
         },
         node_node= {
           setValues(xml_doc,xpath,param_value,ids)
         },

         node_option= {
           setAttrValues(xml_doc,xpath,"choix",param_value,ids)
         },

         form_option= {
           setAttrValues(xml_doc,xpath,"choix",param_value,ids)
         },
         node_table= {
           setValues(xml_doc,xpath,param_value,ids)
         },
         node_attr= {
           setAttrValues(xml_doc,xpath,"nom",param_value,ids)
         },
         attr_attr= {
           setValues(xml_doc,xpath,param_value,ids)
         },
         attr_attr2= {
           value=setAttrValues(xml_doc,xpath, param_name,param_value,ids)
         },
         choix_attr= {
           value=setAttrValues(xml_doc,xpath,param_name,param_value,ids)
         }


  )


  #return(xml_doc)
  return(invisible())
}
