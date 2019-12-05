#' @title Get an example of an XL parameters file into a directory (from package data)
#' @param xl_name name of the XL file or partial match
#' @param dest_dir directory path where to copy the XL file
#'
#' @export
#'

# TODO: summarize with get_script
get_inputs_example <- function(xl_name = NULL, dest_dir = getwd()) {

  package <-  "SticsRFiles"
  inst_dir <- "extdata/xl/"
  xl_patt <- ".(xls|xlsx)$"

  if (base::is.null(xl_name)) {
    xl_name <- xl_patt
  }

  files_list <- list.files(system.file(paste0(inst_dir), package = package),pattern = xl_name)

  if (length(files_list) > 1) {
    warning("You must give a file name, see in returned list !")
    return(files_list)
  }

  if (!length(files_list)) {
    print("No matching files found, see in returned list !")
    return(list.files(system.file(paste0(inst_dir), package = package),pattern = xl_patt))
  }

  dest_list <- file.path(dest_dir, files_list)
  exist_files <- file.exists(dest_list)
  if ( any(exist_files) ) {
    print(paste(files_list[exist_files],"already exists in ", dest_dir))
    return(invisible(FALSE))
  }

  success <- file.copy(from = system.file(paste0(inst_dir,files_list), package = package),to = dest_dir)

  if ( success ) {
    print(paste(files_list," has been copied in directory ", dest_dir))
  }

  return(invisible(success))
}
