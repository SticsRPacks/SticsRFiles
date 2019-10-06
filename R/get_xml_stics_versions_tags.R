#' @export
get_xml_stics_versions_tags <- function(version = "") {

  tmpl_file <- paste0("extdata/xml/templates/")
  versions_tags <- list.dirs(system.file(tmpl_file, package = "SticsRFiles"), full.names = F)

  versions_tags <- versions_tags[! versions_tags == "" ]

  # if ( ! nargs() ) {
  #   return(versions_tags)
  # }

  num_versions <- as.numeric(gsub(pattern = "^[V]","",versions_tags))

  last_version <- versions_tags[ num_versions == max(num_versions) ]

  if ( version == "last") {
    return(last_version)
  }

  versions <- list(versions_list = versions_tags, last_version = last_version )

  return(versions)

}
