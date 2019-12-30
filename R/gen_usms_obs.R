#' Generating observation data files from a data.frame
#'
#' @param obs_table The input onservations data.frame
#' @param out_path The output directory path for storing .obs files
#'
#' @return A return logical status indicating if any error when writing files (FALSE),
#' TRUE when no errors.
#'
#' @examples
#' \dontrun{
#' copy_mailing_example(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' obs_df <- read_excel(xl_path, sheet = "Obs")
#' gen_usms_obs(obs_table = obs_df, out_path = file.path("/path/to/dest/dir","obs"))
#' }
#'
#' @export
#'
gen_usms_obs <- function(obs_table, out_path = getwd()) {


  # Checking if out_path exists
  if ( ! dir.exists(out_path) ) {
    warning(paste("The directory does not exist",out_path ))
    return(invisible(FALSE))
  }

  # Finding usm names column
  usm_idx <- grep("usm",tolower(colnames(obs_table)))
  if ( length(usm_idx) > 1 ) {
    stop("Multiple usms names columns !")
  }

  # Getting usms names
  usm_names <- unique(obs_table[[usm_idx]])
  nb_usms <- length(usm_names)

  # For storing files paths when not generated
  bad_files <- vector(mode = "character", length = nb_usms)

  # Loop over usms names and files generation
  for (i in 1:nb_usms) {
    # Setting usm name
    usm_name <- usm_names[i]

    # Setting the output file path
    out_file_path <- file.path(out_path, paste0(usm_name, ".obs"))

    # Selecting data for the current usm, eliminating all NA values columns
    usm_df <- obs_table %>% dplyr::filter(obs_table[[usm_idx]] %in% usm_name) %>%
      dplyr::select_if(~!all(is.na(.)))

    # Writing the file and
    # storing file path when writing error
    if (! gen_obs(usm_df, out_file_path) ) {
      bad_files[i] <- out_file_path
    }

  }

  # if any error while writing files
  if ( !all(bad_files == "") ) {
    warning(paste("The file has not been generated:",bad_files))
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}
