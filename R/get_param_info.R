#' Finding parameters information using partial search words
#'
#' @param name Optional name or partial name or a vector of
#' @param version Optional, Stics version.
#' Only the 2 last are referenced: V9.0, V9.1 (default value)
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
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_param_info(name = "albedo")
#'
#' get_param_info(name = "albedo", kind = "formalism)
#'
#' get_param_info(name = "albedo", version = "V9.0")
#'
#' get_param_info(name = c("albedo", "latitude", "humcapil"))
#'
#' get_param_info(name = c("albedo", "latitude", "humcapil"),
#' kind = "formalism)
#'
#' }
#'
#'
get_param_info <- function(name = NULL,
                           version=NULL,
                           kind = "all",
                           exact = FALSE) {

  kinds <- c("parameter", "formalism", "all")

  # Checking kind
  if (! kind %in% kinds ) {
    stop(paste("Unknown kind of parameter(s) information to retrieve:",kind))
  }

  # Just in case
  name <- unique(name)

  # Check Stics version
  version <- get_xml_stics_version(version)

  # Getting XML examples files dir from the package
  xml_dir <- get_examples_path( file_type = "xml", version_name = version)

  # Getting the XML files list
  files_list <- list.files(path = xml_dir,
                           pattern = "\\.xml$",
                           full.names = TRUE)

  # Not any files found !
  if (length(files_list) == 0) {
    stop("Examples XML files not found in the package !")
  }

  # Getting parameters names bounds and file
  param_names <- get_param_names_xml(xml_file = files_list,
                                     name = name,
                                     exact = exact)

  # Not any parameters found
  if (all(dim(param_names)==0)) {
    warning(paste("Not any parameter found for Stics version: ", version))
  }

  # Returning parameters information
  if (kind == "parameter") return(param_names)

  # Getting parameter formalism information
  files_list <- file.path(xml_dir, unique(param_names$file))
  param_formalism <- get_formalisms_xml(xml_file = files_list,
                                        par_name = param_names$name)

  param_formalism <- form_list2df(param_formalism)

  # if formalism request
  if (kind == "formalism") return(param_formalism)

  # merging all information from names and formalisms
  param_df <- suppressMessages(dplyr::left_join(param_names,param_formalism))
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
      out_file <- c(out_file,rep(file,length(par_names)))
      out_form <- c(out_form, rep(form,length(par_names) ))
      out_param <- c(out_param, par_names)

    }
    out[[i]] <- data.frame(file = out_file, formalism = out_form, name = out_param, stringsAsFactors = FALSE)
  }

  # returning the data.frame
  dplyr::bind_rows(out)
}

