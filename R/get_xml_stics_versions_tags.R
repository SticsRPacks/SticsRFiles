#' Get all stics XML versions
#'
#' @param last Boolean. Is only the last version needed ?
#'
#' @return All the STICS XML file versions available
#'
#'
get_xml_stics_versions_tags <- function(last= FALSE) {

  tmpl_file <- paste0("extdata/xml/templates/")
  versions_tags <- list.dirs(system.file(tmpl_file, package = "SticsRFiles"), full.names = F)

  versions_tags <- versions_tags[! versions_tags == "" ]

  num_versions <- as.numeric(gsub(pattern = "^[V]","",versions_tags))

  last_version <- versions_tags[ num_versions == max(num_versions) ]

  if (last) {
    return(last_version)
  }

  versions <- list(versions_list = versions_tags, last_version = last_version )

  return(versions)

}
