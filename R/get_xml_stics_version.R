# TODO: A renommer pour clarifier version des fichiers
# a generer et non pas version du modele exe.
# il n'y aura plus de pb des que les fontions seront ventilees
# dans les depots SticsOnR et SticsRFiles sur Github
#' @title Get a version/or the latest version string of available Stics files templates in the package
#'
#' @param stics_version A version key of a Stics version (i.e. V9.1)
#' @param xml_doc an xmlDocument of a Stics xml file (Unused for the moment, no version String included in xml files)
#'
#' @return a Stics version string
#'
#' @examples
#' \dontrun{
#' # View available Stics files version
#' SticsRFiles:::get_xml_stics_version()
#'
#' # View the latest version
#' SticsRFiles:::get_xml_stics_version("latest")
#'
#' # Checking if a version exists
#' SticsRFiles:::get_xml_stics_version("V9.2")
#' }
#'
#' @keywords internal
#'
get_xml_stics_version <- function(stics_version = "latest", xml_doc = NULL) {

  # for the moment the xml_doc does not contain the model version
  # it matches with, only used to avoid checking the version
  # this is a custom case, whatever its content ...

  if (!base::is.null(xml_doc)) {
    # to be fixed when the doc will contain the version
    # return("custom")
  }

  # Getting version from compat csv file
  return(check_version_compat(stics_version = stics_version))
}
