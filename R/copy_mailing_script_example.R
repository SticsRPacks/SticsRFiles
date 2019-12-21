#' @title Get script(s) example(s) for generating xml Stics files from tables (data.frame, tibble)
#' @param in_name name of the script or partial match, or "all" for getting all scripts
#' @param dest_dir directory path where to copy the script
#'
#' @return logical value TRUE if the file copy is successful, FALSE otherwise
#' @export
#'
# TODO: summarize with get_inputs_example
copy_mailing_script_example <- function( in_name = NULL, dest_dir = getwd()) {

  package <-  "SticsRFiles"
  inst_dir <- "extdata/scripts/"

  patt <- "*.R"
  script_name <- in_name
  if ( base::is.null( script_name ) || script_name =="all" ) {
    script_name <- patt
  }


  files_list <- list.files(path = system.file(inst_dir, package = package),pattern = script_name )

  if (base::is.null(in_name)) {
    return(files_list)
  }

  if (!length(files_list)) {
    print("No matching files found, see in returned list !")
    return(list.files(system.file(paste0(inst_dir), package = package),pattern = patt))
  }

  dest_list <- file.path(dest_dir, files_list)
  exist_files <- file.exists(dest_list)
  if ( any(exist_files) ) {
    print(paste(files_list[exist_files],"already exists in ", dest_dir))
    return(invisible(FALSE))
  }


  success <- file.copy(from = system.file(paste0(inst_dir,files_list), package = package),to = dest_dir)

  if ( any(success) ) {
    print(paste(files_list[success]," has been copied in directory ", dest_dir))
  }

  return(invisible(success))

}
