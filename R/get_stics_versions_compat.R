#' Get all stics XML versions
#'
#' @return A list of the STICS versions compatible with this package, and the last version in use.
#'
#' @examples
#' SticsRFiles:::get_stics_versions_compat()
#'
get_stics_versions_compat <- function() {

  # Getting available versions tags
  tmpl_file <- paste0("extdata/xml/templates/")
  versions_tags <- list.dirs(system.file(tmpl_file, package = "SticsRFiles"), full.names = F)

  # Getting versions numbers from directories names
  versions_tags <- versions_tags[! versions_tags == "" ]
  num_versions <- as.numeric(gsub(pattern = "^[V]","",versions_tags))

  # Getting the last version
  last_version <- versions_tags[ num_versions == max(num_versions) ]

  # List of versions tags ans last version tag
  versions <- list(versions_list = versions_tags, last_version = last_version )

  return(versions)

}
