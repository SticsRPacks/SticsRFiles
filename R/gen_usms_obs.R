#' @title Generating observation data files from a data.frame
#'
#' @param obs_table The input onservations data.frame
#' @param out_path A path to an optional folder where to write the .obs files
#' @param usms_list An optional list of usms names to be used for selecting which files to generate
#' from the obs_table
#'
#' @details
#'  `obs_table` is a `data.frame` with the following format:
#'
#'
#' |usm_name       |  ian| mo| jo| jul|densite | lai(n)| masec(n)|   azomes|  ammomes| resmes| AZnit(1)|
#' |:--------------|----:|--:|--:|---:|:-------|------:|--------:|--------:|--------:|------:|--------:|
#' |USM_2017_T1_CI | 2017|  9|  6| 249|NA      |     NA|     0.31| 27.07395| 9.364425|    285| 15.39825|
#' |USM_2017_T1_CI | 2017|  9| 20| 263|NA      |     NA|     0.60| 27.90000| 9.800000|    270|       NA|
#' |USM_2018_T1    | 2017| 10| 20| 293|NA      |    0.1|       NA|       NA|       NA|     NA|       NA|
#' |USM_2018_T1    | 2018|  5| 15| 482|NA      |    1.2|       NA|       NA|       NA|     NA|       NA|
#'
#' * `usm_name` column contains usms names which are used as output .obs files names
#' * `ian`, `mo`, `jo` and `jul` are mandatory (year, month, day and julian date)
#' * Other columns one per variable contain observations values or NA
#'
#'  @seealso \code{\link{get_var_info}} for getting variable right syntax or searching a variable name.
#'
#' @return A return logical status indicating if any error when writing files (FALSE),
#' TRUE when no errors.
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' xl_path <- file.path(get_examples_path("xl"), "inputs_stics_example.xlsx")
#' obs_df <- read_excel(xl_path, sheet = "Obs")
#' gen_usms_obs(obs_table = obs_df, out_path = "/path/to/dest/dir")
#' }
#'
#' @export
#'
gen_usms_obs <- function(obs_table, out_path = getwd(), usms_list = NULL) {


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

  # Treating a usms_list input
  if (!base::is.null(usms_list)) usm_names <- usm_names[usm_names %in% usms_list]

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
