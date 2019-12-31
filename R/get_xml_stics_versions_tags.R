#' Get all stics XML versions
#'
#' @param last Boolean. Is only the last version needed ?
#'
#' @return A list with all the STICS XML file versions available in the package
#' templates and the last version, or only the last one.
#'
#' @examples
#' \dontrun{
#'
#' SticsRFiles:::get_xml_stics_versions_tags()
#'
#' SticsRFiles:::get_xml_stics_versions_tags(last=TRUE)
#'
#' }
#'
#' @keywords internal
#'
get_xml_stics_versions_tags <- function(last = FALSE) {

  # Getting available versions tags
  tmpl_file <- paste0("extdata/xml/templates/")
  versions_tags <- list.dirs(system.file(tmpl_file, package = "SticsRFiles"), full.names = F)

  # Getting versions numbers from directories names
  versions_tags <- versions_tags[! versions_tags == "" ]
  num_versions <- as.numeric(gsub(pattern = "^[V]","",versions_tags))

  # Getting the last version
  last_version <- versions_tags[ num_versions == max(num_versions) ]

  # if the last version wanted
  if (last) {
    return(last_version)
  }

  # List of versions tags ans last version tag
  versions <- list(versions_list = versions_tags, last_version = last_version )

  return(versions)

}
