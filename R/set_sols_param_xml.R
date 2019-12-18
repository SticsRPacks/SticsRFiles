#' @title Setting a sol param value(s) in a sols xmlDocument
#' @param xml_doc_object an xmlDocument object (created from an xml sols file)
#'
#' @param sols_param soils parameters
#' @param overwrite replace existing soil (TRUE) or not, updating existing ones (FALSE)
#'
#@export
#'

set_sols_param_xml <- function(xml_doc_object, sols_param, overwrite = FALSE) {


  if ( ! "data.frame" %in% class(sols_param) ) {
    stop("sols_param do not belong to data.frame class/type")
  }

  if (! "xmlDocument" %in% class(xml_doc_object) ) {
    stop("xml_doc_object is not an XMLDocument object")
  }


  # detecting soils names column
  in_params <- names(sols_param)
  sol_col <- in_params[grep("^soil",tolower(in_params))]
  #in_soils_names <- sols_param[[sol_col]]

  if ( length(sol_col) == 0 || length(sol_col) > 1 ) {
    stop("No soil names column detected, or multiple columns with \"soils\" prefix")
  }

  # checking soils based on names if overwrite == FALSE
  if ( ! overwrite ) {
    # getting soils names
    xml_sols <- as.vector(get_param_value(xml_doc_object,"sol"))
    #sols_idx <- is.element(in_soils_names, xml_sols)

    # checking xl names against xml names
    xl_in_xml <- sols_param[[sol_col]] %in% xml_sols
    if ( ! all(xl_in_xml) ) {
      stop("All sols names in sols_param table are not in sols names in xml doc !")
    }


    # xl sols idx in xml doc to be updated
    sols_xml_idx <- which(xml_sols %in% sols_param[[sol_col]])


    # Selecting data & ordering upon xml
    # order
    sols_param <- sols_param[xl_in_xml,]
    sols_param <- sols_param[match(xml_sols[sols_xml_idx], sols_param[[sol_col]]),]

  } else {
    # setting soils names
    set_param_value(xml_doc_object,"sol",sols_param[[sol_col]])
    #sols_idx <- 1:length(sols_param[[sol_col]])
  }


  # Managing parameter values replacement from sols_param
  # data.frame

  # Treating Layers params
  # reshape them to set values for all soils
  # select epc_1 to epc_5 in df and as.vector to
  # set_sols_param(xml_doc_object, "epc", epc_vec)

  layers_params <- grep("_[0-9]*$",in_params, value = T)
  layers_params_pref <- unique(gsub("_[0-9]*$","",layers_params))

  for (i in 1:5) {
    for (p in layers_params_pref){
      par <- paste0(p,"_",i)
      layer <- paste("layer",as.character(i))
      if (is.element(par,layers_params)) {
        set_param_value(xml_doc_object,p,sols_param[[par]], layer)
      } else {
        delNodes(xml_doc_object,paste0("//tableau[@nom=\"",layer,"\"]"))
      }
    }
  }

  # Treating other params, simple and options
  other_params <- setdiff(in_params, c(layers_params,sol_col))

  # setting param values
  for (p in other_params) {
    set_param_value(xml_doc_object,p,sols_param[[p]])
  }

}
