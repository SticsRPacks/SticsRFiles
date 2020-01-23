#' @title Getting parameters names from xml files
#'
#' @description Extracting parameters names for an xml files or a vector of files.
#'
#' @param xml_file an xml file path or a vector of paths
#'
#' @param output Output data format either "list" or "data.frame" (default)
#'
#' @param combine Logical, usefull only for data.frame.
#' TRUE, to transform a data.frame list to a unique data.frame,
#' FALSE otherwise.
#'
#' @return A list of parameters names data.frames or list, or a unique data.frame
#' for multiple files.
#'
#' @examples
#' \dontrun{
#' library(SticsRFiles)
#' xml_file <- "path/to/xmlfile"
#' xml_files_list <- c("path/to/xmlfile1", "path/to/xmlfile2")
#'
#' param_names <- get_param_names_xml(xml_file)
#'
#' param_names <- get_param_names_xml(xml_files_list)
#' }
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#'
#'
get_param_names_xml <- function(xml_file,
                                bounds = TRUE,
                                output="data.frame",
                                combine = TRUE) {


  output_formats <- c("list", "data.frame")

  # Switch for transformations to data.frame format
  df_out <- output == "data.frame"
  df_comb <- df_out & combine

  # Recusring when multiple files in xml_file
  if ( length(xml_file) > 1 ) {

    param_names <- lapply(xml_file,
                          function(x) get_param_names_xml(xml_file = x,
                                                          output = output,
                                                          combine = combine))
    # To a named list
    param_names <- unlist(param_names, recursive = FALSE)

    # Only for data.frames list
    if (df_comb) {
      param_names <- dplyr::bind_rows(param_names)
    }

    return(param_names)
  }

  # Getting param names for one xml document
  param_names <- list(get_params_names(xml_object = xmldocument(xml_file)))

  if (bounds) {
    param_bounds <- get_param_bounds(xml_doc = xmldocument(xml_file),
                                     param_name = unlist(param_names),
                                     output = output)
  }

  # Transforming list to data.frame (default behaviour)
  if (df_out) {

    param_names <- list(data.frame(file = base::basename(xml_file),
                                   name = unlist(param_names),
                                   stringsAsFactors = FALSE))

    if (bounds) {
      param_names <- merge(param_names, param_bounds)
    }
  } else {

    if (bounds) {
      param_names <- list(file = base::basename(xml_file),param_bounds)
    } else {

      # To a named list
      names(param_names) <- base::basename(xml_file)
    }
  }

  return(param_names)

}
