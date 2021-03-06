#' @title Downloading an Excel usms data file example into a directory
#'
#' @description The file is an example that can be used for generating JavaStics input files
#' from parameters values stored in Excel spreadsheet format (USMs, Ini, Soils, Tec, Station, ...).
#' Each sheet contains parameters values to insert into XML files, with the help of
#' these functions: \code{\link{gen_usms_xml}}, \code{\link{gen_sols_xml}}, \code{\link{gen_tec_xml}},
#' \code{\link{gen_sta_xml}}, \code{\link{gen_usms_xml}}, \code{\link{gen_ini_xml}}
#'
#' @param xl_name Name of an Excel file (optional, not used for the moment)
#' @param dest_dir Directory path where to copy the Excel file
#'  (optional, default: current directory)
#' @param version_name An optional version string (default: last version returned by get_stics_versions_compat())
#' @param overwrite Optional logical, TRUE for overwriting files, FALSE otherwise (default)
#' @param verbose Optional, logical, TRUE for displaying messsages/ warnings,
#' FALSE otherwise (default)
#' @param ... Additional arguments to be passed
#'
#' @return A vector of copied files path.
#'
#' @examples
#' \dontrun{
#' download_usm_xl()
#' download_usm_xl(dest_dir = "/path/to/destination/dir")
#' }
#'
#' @export
#'

download_usm_xl <- function(xl_name = NULL,
                            dest_dir = getwd(),
                            version_name = "last",
                            overwrite = FALSE,
                            verbose = FALSE,
                            ...) {

  oldw <- getOption("warn")
  if(!verbose) {
    options(warn=-1)
  } else {
    options(warn=0)
  }

  args <- list(...)


  xl_patt <- "\\.(xls|xlsx)$"

  if ("type" %in% names(args) && args$type == "csv") xl_patt <- "\\.csv$"

  if (base::is.null(xl_name)) {
    xl_name <- xl_patt
  }

  xl_dir <- get_examples_path( file_type = "xl", version_name = version_name)

  files_list <- list.files(xl_dir,pattern = xl_name)

  if (length(files_list) > 1) {
    warning("You must give a file name, see in returned list !")
    return(files_list)
  }

  if (!length(files_list)) {
    warning("No matching files found, see in returned list !")
    return(list.files(xl_dir, pattern = xl_patt))
  }

  dest_list <- file.path(dest_dir, files_list)
  exist_files <- file.exists(dest_list)
  if ( !overwrite && any(exist_files) ) {
    warning(paste(files_list[exist_files],"already exists in ", dest_dir,
    "\nConsider to set overwrite = TRUE to overwrite (it | them )"))
    #return(invisible(FALSE))
    # filtering existing files, not copied if overwrite == FALSE
    files_list <- files_list[!exist_files]
  }

  src_list <- file.path(xl_dir, files_list)
  success <- file.copy(from = src_list, to = dest_dir, overwrite = overwrite)

  if ( any(success) ) {
    if (verbose) print(paste(files_list[success]," has been copied in directory ", dest_dir))
    # Adding file(s) path as attr
    #attr(success,"path") <- dest_list
    dest_list <- dest_list[success]
  }

  if (!all(success)) warning("Error copying files:\n", paste(src_list[!success], collapse = "\n"))

  options(warn=oldw)

  #return(invisible(success))
  return(invisible(dest_list))
}



#' @title Downloading a CSV usms data file example into a directory
#'
#' @description The file is an example that can be used for generating JavaStics
#' usms.xml input file from parameters values stored in a CSV file using the function
#' \code{\link{gen_usms_xml}}
#'
#' @param csv_name Name of a csv file (optional, not used for the moment)
#' @param dest_dir Directory path where to copy the CSV file
#'  (optional, default: current directory)
#' @param version_name An optional version string (default: last version returned by get_stics_versions_compat())
#' @param overwrite Optional logical, TRUE for overwriting files, FALSE otherwise (default)
#' @param verbose Optional, logical, TRUE for displaying messsages/ warnings,
#' FALSE otherwise (default)
#'
#' @return A vector of copied files path.
#'
#' @examples
#' \dontrun{
#' download_usm_csv(dest_dir = "/path/to/destination/dir")
#' }
#'
#' @export
#'

download_usm_csv <- function(csv_name = NULL,
                            dest_dir = getwd(),
                            version_name = "last",
                            overwrite = FALSE,
                            verbose = FALSE) {


  download_usm_xl(xl_name = csv_name,
                  dest_dir = dest_dir,
                  version_name = version_name,
                  overwrite = overwrite,
                  verbose = verbose,
                  type="csv")

}
