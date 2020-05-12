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
#'
#' @return A copy status, TRUE if successfull, FALSE otherwise.
#' With an attached attribute "path" containing copied files path.
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
                            overwrite = FALSE) {

  xl_patt <- ".(xls|xlsx)$"

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
    print("No matching files found, see in returned list !")
    return(list.files(xl_dir, pattern = xl_patt))
  }

  dest_list <- file.path(dest_dir, files_list)
  exist_files <- file.exists(dest_list)
  if ( !overwrite && any(exist_files) ) {
    print(paste(files_list[exist_files],"already exists in ", dest_dir))
    print("Consider to set overwrite = TRUE to overwrite (it | them )")
    return(invisible(FALSE))
  }

  src_list <- file.path(xl_dir, files_list)
  success <- file.copy(from = src_list, to = dest_dir)

  if ( success ) {
    print(paste(files_list," has been copied in directory ", dest_dir))
    # Adding file(s) path as attr
    attr(success,"path") <- dest_list
  }

  return(invisible(success))
}
