#' @title Get script(s) example(s) for generating xml Stics files from tables (data.frame, tibble)
#' @param in_name Name of the script or partial match, or "all" for getting all scripts
#' @param dest_dir Directory path where to copy the script (optional, default: current directory)
#' @param overwrite Logical value to overwrite files (TRUE) or not (FALSE, default)
#'
#' @return Logical value TRUE if the file copy is successful, FALSE otherwise
#'
#' @examples
#'
#' \dontrun{
#' # getting files list
#' copy_mailing_script_example()
#' [1] "generate_ini_files.R" "generate_sols_file.R" "generate_sta_files.R" "generate_tec_files.R"
#' [5] "generate_usms_file.R"
#' # copying all scripts
#' copy_mailing_script_example("all")
#' # copying ini and sols scripts
#' copy_mailing_script_example("ini", "sols")
#' }
#'
#' @export
#'
# TODO: summarize with get_inputs_example
copy_mailing_script_example <- function( in_name = NULL,
                                         dest_dir = getwd(),
                                         overwrite = F) {

  package <-  "SticsRFiles"
  inst_dir <- "extdata/scripts"

  patt <- "*.R"
  script_name <- in_name
  if ( base::is.null( script_name ) || script_name =="all" ) {
    script_name <- patt
  }

  # Scripts files list
  files_list <- list.files(path = system.file(inst_dir, package = package),
                           pattern = patt )

  # Returning the full script files list
  if (base::is.null(in_name)) {
    return(files_list)
  }

  # Filtering files
  files_list <- unlist(lapply(script_name,
                              function(x) grep(x = files_list, pattern = x, value = T)))

  if (!length(files_list)) {
    print("No matching files found, see in returned list !")
    return(list.files(system.file(paste0(inst_dir), package = package),pattern = patt))
  }

  # Checking if files already exist in target directory
  dest_list <- file.path(dest_dir, files_list)
  exist_files <- file.exists(dest_list)
  if ( any(exist_files) && ! overwrite) {
    print(paste(files_list[exist_files],"already exists in ", dest_dir))
    #return(invisible(FALSE))
    if (! overwrite) files_list <- files_list[! exist_files]
  }

  # Getting copy status of files
  files_path <- file.path(inst_dir,files_list)
  success <- file.copy(from = system.file(files_path,
                                          package = package),
                       to = dest_dir,
                       overwrite = overwrite)

  # Copy message according to copy status
  if ( any(success) ) {
    print(paste(files_list[success]," has been copied in directory ", dest_dir))
  }

  # Returning copy status vector
  return(invisible(success))

}
