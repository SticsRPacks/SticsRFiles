#' Read STICS observation files (*.obs) and store data into a list
#'
#' @description Read STICS observation files for sole crops and store data into a list
#'
#' @param dirpath  Directory path
#' @param obs_filenames A vector of observation file name(s). Optional, see details.
#' @param usms     A vector of usm name(s) corresponding the the obs files. Optional, see details.
#' @param usms_filename name of the usm file. Optional, see details.
#'
#' @details If \code{filename} is not specified (or equal to \code{NULL}), the
#'          function read all obs files in the \code{dirpath} folder. If there are no .obs files,
#'          the function returns \code{NULL}
#'          If usms names are not provided they are supposed to have the same name as the obs file.
#'          If usms_filename is not provided, existence of usms are checked in the usms.xml file.
#'
#'
#' @return A list of STICS-formated observations. Return \code{NULL} if no files were found.
#'
#' @seealso \code{\link{get_obs_int}}
#'
#'
#' @examples
#' \dontrun{
#' path <- system.file(file.path("extdata","obs","V9.0"), package = "SticsRFiles")
#' Meas= get_obs(path)
#'}
#'
#@export
#'
#' @keywords internal
#'
read_obs= function(dirpath=getwd(), obs_filenames=NULL, usms=NULL, usms_filename="usms.xml"){

  return(get_obs(dirpath = dirpath,
                 obs_filenames = obs_filenames,
                 usms = usms,
                 usms_filename = usms_filename))
}
