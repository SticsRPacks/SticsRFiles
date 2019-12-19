# TODO: A renommer pour clarifier version des fichiers
# a generer et non pas version du modele exe.
# il n'y aura plus de pb des que les fontions seront ventilees
# dans les depots SticsOnR et SticsRFiles sur Github
#' @title Get a version/or the laste version string of available Stics files templates in the package
#'
#' @param stics_version A version key of a Stics version (i.e. V9.0)
#' @param xml_doc an xmlDocument of a Stics xml file (Unused for the moment, no version String included in xml files)
#'
#' @return a Stics version string
#'
#' @keywords internal
#'
get_xml_stics_version <- function(stics_version = "last", xml_doc = NULL) {

  # for the moment the xml_doc does not contain the model version
  # it matches with, only used to avoid checking the version
  # this is a custom case, whatever its content ...

  if (! base::is.null( xml_doc)) {
    # to be fixed when the doc will contain the version
    return("custom")
  }

  known_versions <- get_xml_stics_versions_tags()

  if ( stics_version =="last" ) {
    stics_version <- known_versions$last_version
  }

  if ( ! stics_version %in% known_versions$versions_list ) {
    stop(paste0("The Stics version is unknown : ", stics_version))
  }

  return(stics_version)
}
