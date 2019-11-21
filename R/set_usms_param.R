#' @title Setting a usm param value(s) in an usms xmlDocument
#' @param xml_doc_object an xmlDocument object (created from an usms file)
#'
#' @param usms_param usms parameters
#' @param overwrite replace existing soil (TRUE) or not, updating existing ones (FALSE)
#'
#' @export
#'

set_usms_param <- function(xml_doc_object, usms_param = NULL, overwrite = FALSE) {


  if ( ! is.null(usms_param) && ! ("data.frame" %in% class(usms_param)) ) {
    stop("usms_param does not belong to data.frame class/type")
  }

  if (! "xmlDocument" %in% class(xml_doc_object) ) {
    stop("xml_doc_object is not an XMLDocument object")
  }

  # detecting usm names column
  in_params <- names(usms_param)
  usm_col <- grep("^usm",in_params, value = T)
  #in_usms_names <- usms_param[[usm_col]]

  if ( length(usm_col) == 0 || length(usm_col) > 1 ) {
    stop("No usm names column detected, or multiple columns with \"usm\" prefix")
  }

  # checking usms based on names if overwrite == FALSE
  if ( ! overwrite ) {
    # getting usm names
    xml_usms <- as.vector(get_param_value(xml_doc_object,"usm"))

    ###############################################
    # TODO : see adding usms not in xml file ?
    ###############################################


    # checking xl names against xml names
    xl_in_xml <- usms_param[[usm_col]] %in% xml_usms
    if ( ! all(xl_in_xml) ) {
      stop("All usms names in usms_param table are not in usms names in xml doc !")
    }

    # xl usms idx in xml doc to be updated
    usms_xml_idx <- which(xml_usms %in% usms_param[[usm_col]])


    # Selecting data & ordering upon xml
    # order
    usms_param <- usms_param[xl_in_xml,]
    usms_param <- usms_param[match(xml_usms[usms_xml_idx], usms_param[[usm_col]]),]

  } else {
    # setting usms names
    set_param_value(xml_doc_object,"usm",usms_param[[usm_col]])
    usms_xml_idx <- 1:length(usms_param[[usm_col]])
  }


  # Managing parameter values replacement from usms_param
  # data.frame

  # Getting param names linked to plante node
  plante_params <- grep("_[0-9]*$",in_params, value = T)
  plante_params_pref <- unique(gsub("_[0-9]*$","",plante_params))

  # Setting parameter values
  for (i in 1:2) {
    for (p in plante_params_pref){
      par <- paste0(p,"_",i)
      if (is.element(par,plante_params)) {
        set_param_value(xml_doc_object,p,
                        usms_param[[par]],
                        "plante",as.character(i),
                        usms_xml_idx)
      }
    }
  }

  # Getting independant params from plante
  other_params <- setdiff(in_params, c(plante_params,usm_col))

  # Setting param values
  for (p in other_params) {
    set_param_value(xml_doc_object,p,usms_param[[p]],
                    usms_xml_idx)
  }

}
