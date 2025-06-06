#' @title Getting parameter values from xml files
#'
#' @description Extracting parameter values for a list of xml files and
#' parameters
#'
#' @param file Vector of the xml file paths from which parameters values
#' must be extracted
#' @param param Vector of parameter names. Optional, if not provided, the
#' function returns information for all parameters.
#' @param select node name or attribute name to use for selection
#' (optional, default to no selection)
#' @param select_value Vector of values used for select (see examples).
#' Optional, should be provided only if select is provided.
#' @param value_id Vector of ids of the parameters values to be retrieved
#' from the parameter values vector
#' @param ... Pass further arguments to `get_param_value()`
#'
#' @return A list of parameter values for each file (a list of list)
#'
#' @examples
#'
#' # Soil file
#' file <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#'
#' # For all soils
#' get_param_xml(file)
#' get_param_xml(file, c("argi", "norg"))
#'
#' # With soil selection
#' # scalar parameters per soil
#' get_param_xml(file, c("argi", "norg"),
#'   select = "sol", select_value = c("solcanne", "solbanane")
#' )
#'
#' # Crop management file
#' file <- file.path(get_examples_path(file_type = "xml"), "file_tec.xml")
#'
#' # Getting parameters for irrigation (date and quantity)
#' get_param_xml(file, c("julapI_or_sum_upvt", "amount"))
#'
#' @export
get_param_xml <- function(
  file,
  param = NULL,
  select = NULL,
  select_value = NULL,
  value_id = NULL,
  ...
) {
  # ... argument for passing : ids, show_xpath to get_param_value

  xml_docs <- lapply(file, xmldocument)

  # Checking if any param duplicates in tec files, for 'cut crop' choices
  lapply(
    xml_docs,
    function(x) {
      check_choice_param(
        xml_doc = x,
        param_name = param
      )
    }
  )

  values <- get_param_value(
    xml_doc = xml_docs,
    param_name = param,
    parent_name = select,
    parent_sel_attr = select_value,
    ids = value_id,
    ...
  )
  xml_names <- lapply(file, basename) %>% unlist()

  # If there are duplicated names in file
  is_duplicated_name <- xml_names %>% duplicated()
  xml_names[is_duplicated_name] <- paste0(
    "xml_",
    which(is_duplicated_name == TRUE),
    "_",
    xml_names[is_duplicated_name]
  )

  names(values) <- xml_names

  lapply(xml_docs, delete)

  values
}
