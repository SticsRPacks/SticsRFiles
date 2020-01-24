#' @title Getting parameters bounds from an xmlDocument object
#'
#' @description Extracting parameters min and/or max bounds for a parameter
#' or a vector of parameters from an xmlDocument class object.
#'
#' @param xml_doc an xmlDocument class object
#' @param param_name a parameter name of a vector of parameters names
#' @param bounds_names bound(s) name(s) "min" or "max" (optional, default value c("min","max ))
#' @param output Output data format either "list" or "data.frame" (default)
#'
#' @return A data.frame with the name of the parameter and min, max values or
#'
#' one of them. Or a named list (with parameter name) containing a named vector
#' for bounds.
#'
#' @examples
#' \dontrun{
#' xml_sta <- system.file(paste0("extdata/xml/examples/V9.0/file_sta.xml"),
#' package = "SticsRFiles")
#'
#' sta_doc <- SticsRFiles:::xmldocument(xml_sta)
#'
#' par_bounds <- SticsRFiles:::get_param_bounds(sta_doc,"zr")
#'
#' par_bounds_list <- SticsRFiles:::get_param_bounds(sta_doc,c("zr","altistation"))
#'
#'
#' SticsRFiles:::get_param_bounds(sta_doc,c("zr","altistation"),"min")
#'
#' }
#'
#' @importFrom dplyr bind_rows
#'
get_param_bounds <- function(xml_doc,
                             param_name,
                             bounds_names = NULL,
                             output = "data.frame") {


  output_formats <- c("list", "data.frame")
  def_names <- c("min","max")

  df_out <- output == "data.frame"

  # Just in case ...get unique bounds names
  bounds_names <- unique(bounds_names)

  if (base::is.null(bounds_names)) {
    bounds_names <- def_names
  } else if ( ! all(is.element(bounds_names,def_names )) ) {
    stop("Names are not consitent with file bound names")
  }

  if ( length(param_name) > 1) {

    bounds_list <- lapply(as.list(param_name),
                          function(x) get_param_bounds(xml_doc = xml_doc,
                                                       x,
                                                       bounds_names = bounds_names,
                                                       output = output))



    if (df_out) {
      bounds_list <- dplyr::bind_rows(bounds_list)
    } else {
      bounds_list <- unlist(bounds_list,recursive = F )
    }

    return(bounds_list)
  }

  param_type <- get_param_type(xml_doc, param_name)
  exist_param <- !base::is.null(param_type$xpath)

  if (! exist_param ) return(NULL)

  xpath <- param_type$xpath

  # for an option parameter, getting
  if ( param_type$type =="option") {
    xpath <- paste0(xpath,"/choix")
    tmp_values <- as.numeric(getAttrsValues(xml_doc, xpath, "code"))
    bounds_values <- c(min(tmp_values),
                       max(tmp_values))[def_names == bounds_names]

  } else {
    # for all other parameters ?
    # Adding unique for repeated parameters : for exmaple in sols.xml
    bounds_values <- lapply(bounds_names,
                            function(x) as.numeric(getAttrsValues(xml_doc, xpath, x)))
  }


  #print(xpath)

  # Fixing bounds values
  bounds_values <- list(fix_bounds(bounds_values, bounds_names))
  names(bounds_values) <- param_name

  # For returning a data.frame
  if (df_out) {
    bounds_values <- t(as.data.frame(bounds_values))
    bounds_values <- data.frame(name = param_name,
                                bounds_values,
                                stringsAsFactors = FALSE)
    rownames(bounds_values) <- NULL
  }

  return(bounds_values)

}

fix_bounds <- function(values, bounds_names) {

  values <- fix_dup_bounds(values, bounds_names)

  values <- fix_missing_bounds(values, bounds_names)

  #if (all(base::is.na(values))) return(values)



  return(values)
}



#' Setting bounds values (min, max) for missing bounds and/or
#' naming bounds values vector with bounds names
#'
#' @param values Vector of bounds values (may be NULL or empty value)
#' @param bounds_names Character vector of bounds names (in "min", "max"
#' one of them or both)
#'
#' @return A named vector for bound(s)
#'
#' @keywords internal
#'
#'
#'
fix_missing_bounds <- function(values, bounds_names) {

  # Fixing missing values
  val <- unlist(values)

  if ( ! base::is.null(val) && length(val) != 0 ) {
    values <- as.numeric(val)
  } else {
    values <- c(NA, NA)[1:length(bounds_names)]
  }

  names(values) <- bounds_names

  return(values)

}


fix_dup_bounds <- function(values, bounds_names) {


  # Fixing duplicates
  values <- lapply(values, unique)
  duplicates <- unlist(lapply(values, function(x) length(x) > 1 ))

  if (! any(duplicates)) return(values)

  for (i in 1:length(duplicates)){
    if (duplicates[i] && bounds_names[i] == "min") {
      values[[i]] <- min(values[[i]])
    }

    if (duplicates[i] && bounds_names[i] == "max") {
      values[[i]] <- max(values[[i]])
    }
  }

  if (any(duplicates)) {
    warning(paste("Found duplicate values in bound(s) \n",
                  paste(bounds_names[duplicates], collapse = ", "),
                  "\n","They have been replaced with min or max values !\n",
                  paste(sprintf("%s: %s",bounds_names[duplicates],
                        as.character(values[duplicates])), collapse = "\n")))
  }

  return(values)
}


