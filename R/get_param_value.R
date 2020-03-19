#' @title Get a set of a (or a list of) Stics xml parameter(s) values from a request
#' @title in an xmlDocument or a list of
#' @title Getting parameter value for different kinds of parameters
#'
#' @description Extracting parameter value from an xml document object
#'
#' @param xml_doc an xmlDocument object
#' @param param_name parameter name or a vector of names (optional)
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ... To pass some other arguments
#'
#' @return a numeric vector values of parameter or a list of
#'
#' @examples
#'
#' \dontrun{
#' xml_path = system.file("extdata/xml/examples/V9.1/sols.xml", package = "SticsRFiles")
#' sols_doc <- SticsRFiles:::xmldocument(xml_path)
#' SticsRFiles:::get_param_value(sols_doc, "argi")
#' SticsRFiles:::get_param_value(sols_doc, c("argi", "norg"))
#'
#' SticsRFiles:::get_param_value(sols_doc, "argi",
#' parent_name = "sol", parent_sel_attr = "solcanne")
#'
#' SticsRFiles:::get_param_value(sols_doc, c("argi", "norg"),
#' parent_name = "sol", parent_sel_attr = c("solcanne", "solbanane"))
#'
#' SticsRFiles:::get_param_value(list(sols_doc, sols_doc), c("argi","norg"),
#' parent_name = "sol", parent_sel_attr = c("solcanne","solbanane"))
#' }
#'
#' @keywords internal
#'

get_param_value <- function(xml_doc,
                            param_name=NULL,
                            parent_name = NULL,
                            parent_sel_attr = NULL,
                            ...) {


  # TODO :
  # 1 - set default param_name value to NULL and calculate le param_name vector calling
  # the get_param_names function !

  # Manage ids and show_xpath and remove them from the list
  ids <- NULL
  show_xpath <- FALSE

  # ... for getting : ids, show_xpath arguments
  dot_args= list(...)
  dot_names <- names(dot_args)

  # Getting ids and show_xpath
  if ("ids" %in% dot_names) ids <- dot_args$ids
  if ("show_xpath" %in% dot_names) show_xpath <- dot_args$show_xpath

  # Getting parent informations to put them in the query
  # parent_idx <- dot_names %in% c("ids", "show_xpath")
  # dot_args <- dot_args[!parent_idx]
  # parent_list <- get_param_parent(dot_args)
  #
  # if (length(dot_args) && base::is.null(parent_list$parent_name)){
  #   warning(paste0("Not any possible selection using ", names(dot_args),
  #                  " with value \"",dot_args[[names(dot_args)]],"\" ",
  #                  "for parameter named ", param_name))
  #   return(NULL)
  # }



  # Getting param values for the same parameters for the xml documents list
  if ( base::is.list((xml_doc)) ) {
    values <- lapply(xml_doc, function(x) get_param_value(xml_doc = x,
                                                          param_name = param_name,
                                                          parent_name = parent_name,
                                                          parent_sel_attr = parent_sel_attr,
                                                          ...))
    return(values)
  }

  # If no given parameters names
  if (base::is.null(param_name)) param_name <- get_param_names(xml_doc)

  # only one parameter
  # if (length(param_name) == 1) {
  #   param_value <- list(get_param_value(xml_doc = xml_doc,
  #                   param_name = param_name,
  #                   parent_name = parent_name,
  #                   parent_sel_attr = parent_sel_attr,
  #                   ...))
  #   names(param_value) <- param_name
  #   return(param_value)
  # }

  # recursive call for a parameter name list
  if (length(param_name) > 1) {
    param_value <- lapply(param_name, function(x) get_param_value(xml_doc = xml_doc,
                                                          param_name = x,
                                                          parent_name = parent_name,
                                                          parent_sel_attr = parent_sel_attr,
                                                          ...))
    names(param_value) <- param_name
    return(param_value)
  }


  # Fixing parent's information
  #parent_name <- parent_list$parent_name
  #parent_sel_attr <- parent_list$parent_sel_attr

  # Getting param type and path
  param_type <- get_param_type(xml_doc = xml_doc,
                               param_name = param_name,
                               parent_name = parent_name,
                               parent_sel_attr = parent_sel_attr,
                               id = ids)
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
         },
         nodename_childs= {
           value=getValues(xml_doc,xpath,ids)
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



get_param_parent <- function(args_list){
  #dot_args= list(...)
  parent_name <- NULL
  parent_sel_attr <- NULL

  #if (!length(dot_args)) stop("Not any args !")

  if (length(args_list) > 1) stop("Too much args !")


  # Argument names to be detected
  #arg_names <- c("soil", "plant", "variete", "usm")
  files_types <- c("sols", "ini", "plt", "usms")
  parent_names <- c("sol", "plante", "variete", "usm")

  parent_list <- list(parent_name = parent_name,
                      parent_sel_attr = parent_sel_attr)


  #parent_idx <- arg_names %in% names(args_list)
  parent_idx <- parent_names %in% names(args_list)

  if (!any(parent_idx)) {
    #warning(paste("Unknown name for ",as.character(args_list[[1]]), "value"))
    return(parent_list)
  }

  parent_list$parent_name <- parent_names[parent_idx]
  parent_list$parent_sel_attr <- as.character(args_list[[1]])

  return(parent_list)
}
