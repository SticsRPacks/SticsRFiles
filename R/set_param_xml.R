#' @title Setting parameter values into xml files
#'
#' @description Setting parameter values for a parameter or a vector of and
#' with a parameters values vector
#'
#' @param file Path (including name) of the xml file to modify
#' @param param Vector of parameter names.
#' @param values A vector or a list of parameter(s) values (see details).
#' @param save_as Path (including name) of the xml file to generate.
#' Optional, if NULL `file` is overwritten.
#' @param select node name or attribute name to use for selection
#' (optional, default to no selection)
#' @param select_value Vector of values used for select (see examples).
#' Optional, should be provided only if select is provided.
#' @param value_id Vector of ids of the parameters values to be retrieved
#' from the parameter values vector
#' @param overwrite Logical TRUE for overwriting the output file,
#' FALSE otherwise (default)
#' @param... Pass further arguments to `set_param_value()`.
#'
#' @return A logical value TRUE for operation success, FALSE otherwise
#'
#' @details It is possible to give several values for a parameter by passing
#' a vector of values. For example, for two parameters with two values each:
#' value= list(c(1,2), c(2.3,4.5))
#'
#' @examples
#'
#' ex_path <- get_examples_path(file_type = "xml")
#'
#' # Soil file
#'
#' sol_path <- file.path(ex_path, "sols.xml")
#'
#' # For scalar parameters per soil
#' # Setting all soils "argi" values to 50
#' set_param_xml(sol_path, "argi", 50, overwrite = TRUE)
#' # Getting changed values
#' # get_param_xml(sol_path, "argi")
#'
#' # Setting a specific value to "argi" for "solcanne" soil
#' set_param_xml(
#'   file = sol_path, param = "argi", values = 56,
#'   select = "sol", select_value = "solcanne", overwrite = TRUE
#' )
#' # Getting changed values
#' # get_param_xml(sol_path, "argi",
#' #   select = "sol", select_value = "solcanne"
#' # )
#'
#'
#' # Setting a specific values to 2 parameters "argi" and
#' # "norg" for "solcanne" soil
#' set_param_xml(sol_path, c("argi", "norg"), list(100, 150),
#'   select = "sol", select_value = "solcanne", overwrite = TRUE
#' )
#' # Getting changed values
#' # get_param_xml(sol_path, c("argi", "norg"),
#' #   select = "sol", select_value = "solcanne"
#' # )
#'
#'
#' # For vector parameters per soil (5 values, one per soil layer)
#' set_param_xml(sol_path, c("epc", "HCCF"),
#'   select = "sol",
#'   select_value = c("solcanne", "solbanane"),
#'   values = list(c(20:24, 10:14), c(50:54, 40:44)),
#'   overwrite = TRUE
#' )
#'
#' # Getting changed values
#' # get_param_xml(sol_path, c("epc", "HCCF"),
#' # select = "sol",
#' # select_value = c("solcanne", "solbanane")
#' # )
#'
#' # For specific values of vector parameters
#' set_param_xml(sol_path, "HCCF",
#'   select = "sol",
#'   select_value = "solcanne",
#'   values = c(46.8, 48.5, 50.1),
#'   value_id = c(1, 3, 5),
#'   overwrite = TRUE
#' )
#'
#' # Getting changed values
#' # get_param_xml(sol_path, "HCCF",
#' # select = "sol",
#' # select_value = "solcanne",
#' # value_id = c(1,3,5)
#' # )
#'
#' # Crop management file
#'
#' tec_path <- file.path(ex_path, "file_tec.xml")
#'
#' # Modifying irrigations parameters
#' set_param_xml(tec_path, c("julapI_or_sum_upvt", "amount"),
#'   values = list(200:215, 20:35), overwrite = TRUE
#' )
#' # Getting changed values
#' # get_param_xml(tec_path, c("julapI_or_sum_upvt", "amount"))
#'
#' @export
set_param_xml <- function(
    file,
    param,
    values,
    save_as = NULL,
    select = NULL,
    select_value = NULL,
    value_id = NULL,
    overwrite = FALSE,
    ...) {
  # ... argument for passing : ids, show_xpath to get_param_value

  # Setting output file path
  if (base::is.null(save_as)) {
    if (overwrite) {
      save_as <- file
    } else {
      warning(
        "The output file path is NULL, so the file will not be saved",
        " and the original file will not be overwritten (overwrite is FALSE)."
      )
      return(invisible(FALSE))
    }
  } else {
    if (overwrite) {
      warning(
        "The output file path is not NULL, but the file will be overwritten.",
        " Set overwrite argument to FALSE to avoid overwriting."
      )
      return(invisible(FALSE))
    }
  }

  # For future version
  # TODO: multiple files and multiple params list and values ...ids ...?
  xml_doc <- xmldocument(file)

  # Checking if any of param can be in intervention
  # nodes of 2 option choices (specific of "cut crop" in tec files)
  check_choice_param(
    xml_doc = xml_doc,
    param_name = param,
    stop = TRUE
  )

  # Setting parameters values in the xmlDocument object
  set_param_value(
    xml_doc,
    param_name = param,
    param_value = values,
    parent_name = select,
    parent_sel_attr = select_value,
    ids = value_id,
    ...
  )

  # Saving
  save_xml_doc(xml_doc, save_as)

  delete(xml_doc)

  # Output status
  invisible(TRUE)
}
