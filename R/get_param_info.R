#' Finding parameters information using partial search words
#'
#' @param param Vector of parameter names (or partial names). Optional, if not
#' provided, the function returns information for all parameters
#'
#' @param parameter `r lifecycle::badge("deprecated")` `parameter` is no
#'   longer supported, use `param` instead.
#'
#' @param file Vector of xml file paths. Optional, if not provided, the function
#' searches information in all standard XML files.
#' @param file_path `r lifecycle::badge("deprecated")` `file_path` is no
#'   longer supported, use `file` instead.
#'
#' @param formalism Optional, formalism name or partial name, or a vector of
#'
#' @param keyword Optional, strings or a vector of to be used for searching
#' in parameters data (i.e.: parameters names, formalisms description,
#' file names or part to which parameters are attached to)
#'
#' @param stics_version Name of the Stics version. Optional, can be used to search
#' parameters information relative to a specific Stics version. By default the
#' latest version returned by `get_stics_versions_compat()` is used.
#'
#' @param version `r lifecycle::badge("deprecated")` `version` is no
#'   longer supported, use `stics_version` instead.
#'
#' @details parameter and formalism may be both set or only one of them, but
#' none of them can be if keyword argument is used.
#'
#' @return A data.frame containing parameters names,
#' their file name origin, their bounds and the formalism they belong to. The
#' data.frame has the model version as attribute.
#'
#' @export
#'
#' @examples
#'
#' get_param_info(param = "albedo")
#' \dontrun{
#' get_param_info(param = "albedo", file = "/path/to/file.xml")
#'
#' get_param_info(param = "albedo", formalism = "special")
#'
#' get_param_info(param = "albedo", stics_version = "V9.0")
#'
#' get_param_info(param = c("alb", "lat"))
#'
#' get_param_info(keyword = "tec")
#'
#' # Get the model version afterward:
#' params <- get_param_info(param = "albedo")
#' attr(params, "version")
#' }
#'
get_param_info <- function(param = NULL,
                           file = NULL,
                           formalism = NULL,
                           keyword = NULL,
                           stics_version = "latest",
                           file_path = lifecycle::deprecated(),
                           parameter = lifecycle::deprecated(),
                           version = lifecycle::deprecated()) {

  # Managing the parameter name changes from 0.5.0 and onward:
  if (lifecycle::is_present(file_path)) {
    lifecycle::deprecate_warn("1.0.0", "get_param_info(file_path)", "get_param_info(file)")
  } else {
    file_path <- file # to remove when we update inside the function
  }

  if (lifecycle::is_present(parameter)) {
    lifecycle::deprecate_warn("1.0.0", "get_param_info(parameter)", "get_param_info(param)")
  } else {
    parameter <- param # to remove when we update inside the function
  }

  if (lifecycle::is_present(version)) {
    lifecycle::deprecate_warn("1.0.0", "get_param_info(version)", "get_param_info(stics_version)")
  } else {
    version <- stics_version # to remove when we update inside the function
  }

  # Defining compatible cases
  # param and/or formalism may be not NULL
  # if keyword is not NULL param and formalism must be NULL
  # or if param and/or formalism are/is not NULL keyword must be NULL
  par_use <- !base::is.null(parameter)
  form_use <- !base::is.null(formalism)
  parform_use <- all(c(par_use, form_use))
  keyword_use <- !base::is.null(keyword)

  if (all(c(any(parform_use), keyword_use))) {
    stop("Incompatible search using both parameter and/or formalism words and keywords !")
  }


  # Getting data when only searching in parameters name
  # or all other cases
  if (par_use & !form_use) {
    param_data_df <- suppressWarnings(get_param_data_df(
      file = file_path,
      param = parameter,
      stics_version = version,
    ))
  } else {
    param_data_df <- suppressWarnings(get_param_data_df(
      file = file_path,
      stics_version = version
    ))
  }


  # Extract data without filtering
  # or filtering done in the previous step
  # when generating param_data_df
  if (!any(c(par_use, form_use, keyword_use)) |
      (par_use & !form_use)) {
    return(param_data_df)
  }

  # Searching in all columns
  if (keyword_use) {
    param_names <- find_names(names = param_data_df$name, name = keyword)
    form_names <- find_names(names = param_data_df$formalism, name = keyword)
    file_names <- find_names(names = param_data_df$file, name = keyword)

    lines_filter <- param_data_df$formalism %in% form_names | param_data_df$file %in% file_names |
      param_data_df$name %in% param_names

    return(dplyr::filter(param_data_df, lines_filter))
  }

  # getting formalism names
  form_names <- find_names(names = param_data_df$formalism, name = formalism)

  # Only searching in formalism
  if (!par_use & form_use) {
    return(dplyr::filter(param_data_df, param_data_df$formalism %in% form_names))
  }

  # Searching in both name and formalism
  if (parform_use) {
    param_names <- find_names(names = param_data_df$name, name = parameter)
    lines_filter <- param_data_df$formalism %in% form_names |
      param_data_df$name %in% param_names
    return(dplyr::filter(param_data_df, lines_filter))
  }
}


#' Getting parameters information using partial search words
#'
#' @param param Optional name or partial name or a vector of
#'
#' @param file Optional, xml file path or a vector of
#'
#' @param stics_version An optional version name as listed in
#' get_stics_versions_compat() return
#' @param kind Kind of information to be retrieved for parameters
#' among "parameter", "formalism" or "all" for both of them
#'
#' @param exact Logical, if TRUE, the exact name is searched
#'
#' @details If not any name vector of names, information are extracted for
#' all the existing parameters
#'
#' @return A data.frame containing parameters names and
#' their origin (file name), and their bounds or the formalism they belong to,
#' or both of them.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' get_param_data_df(param = "albedo")
#'
#' get_param_data_df(param = "albedo", file = "/path/to/file.xml")
#'
#' get_param_data_df(param = "albedo", kind = "formalism")
#'
#' get_param_data_df(param = "albedo", stics_version = "V9.0")
#'
#' get_param_data_df(param = c("albedo", "latitude", "humcapil"))
#'
#' get_param_data_df(
#'   param = c("albedo", "latitude", "humcapil"),
#'   kind = "formalism"
#' )
#'
#' get_param_data_df( file = "/path/to/JavaSTICS/folder/config/inputs.csv")
#'
#' get_param_data_df( param = "albedo",
#'    file = "/path/to/JavaSTICS/folder/config/inputs.csv")
#'
#'}
#'
get_param_data_df <- function(param = NULL,
                              file = NULL,
                              stics_version = "latest",
                              kind = "all",
                              exact = FALSE) {

  kinds <- c("parameter", "formalism", "all")

  # Checking kind
  if (!kind %in% kinds) {
    stop(paste("Unknown kind of parameter(s) information to retrieve:", kind))
  }

  # Just in case
  param <- unique(param)

  if (base::is.null(file)) {
    # Check Stics version
    stics_version <- get_xml_stics_version(stics_version)

    # Getting XML examples files dir from the package
    xml_dir <- get_examples_path(file_type = "xml", stics_version = stics_version)

    # Getting the XML files list
    files_list <- list.files(
      path = xml_dir,
      pattern = "\\.xml$",
      full.names = TRUE
    )

    # Not any files found !
    if (length(files_list) == 0) {
      stop("Examples XML files not found in the package !")
    }
  } else {
    files_list <- file
  }

  # getting parameters from an inputs.csv file
  if( length(files_list) == 1 && grepl( pattern = "inputs.csv", x = files_list)) {
    param_names <- utils::read.csv2(
      file,
      header = FALSE,
      stringsAsFactors = FALSE
    )[c(1,4,5,7:8)]
    names(param_names) <- c("name", "file", "dim", "min", "max")

    if(is.null(param)) return(param_names)

    par_idx <- param_names$name %in% param

    return(param_names[par_idx, ])
  }

  # Getting parameters names bounds and file
  param_names <- suppressWarnings(get_param_names_xml(
    xml_file = files_list,
    name = param,
    exact = exact
  ))

  # Not any parameters found
  if (dim(param_names)[1] < 1) {
    warning(paste("Not any parameter found for Stics version: ", stics_version))
    return(invisible())
  }

  # Returning parameters information
  if (kind == "parameter") {
    return(param_names)
  }

  # Some parameters names may be found in several files (i.e.: nbplantes)
  uniq_param_names <- unique(param_names$name)

  param_formalism <- get_formalisms_xml(
    xml_file = files_list,
    par_name = uniq_param_names
  )

  param_formalism <- form_list2df(param_formalism)

  # if formalism request
  if (kind == "formalism") {
    return(param_formalism)
  }

  # merging all information from names and formalisms
  param_df <- suppressMessages(dplyr::left_join(param_names, param_formalism))

  # Adding a version  attribute
  attr(x = param_df, which = "version") <- stics_version
  return(param_df)
}



form_list2df <- function(formalism_list) {

  # filtering NA list values (files whithout formalisms)
  formalism_list <- formalism_list[unlist(lapply(formalism_list, is.list))]

  # files names
  files <- names(formalism_list)

  out <- vector("list", length(files))

  for (i in 1:length(files)) {
    file <- files[i]
    forms_names <- names(formalism_list[[file]])
    out_file <- NULL
    out_form <- NULL
    out_param <- NULL
    for (j in 1:length(forms_names)) {
      form <- forms_names[[j]]
      par_names <- formalism_list[[file]][[j]]
      out_file <- c(out_file, rep(file, length(par_names)))
      out_form <- c(out_form, rep(form, length(par_names)))
      out_param <- c(out_param, par_names)
    }
    out[[i]] <- data.frame(file = out_file, formalism = out_form, name = out_param, stringsAsFactors = FALSE)
  }

  # returning the tibble
  dplyr::as_tibble(dplyr::bind_rows(out))
}
