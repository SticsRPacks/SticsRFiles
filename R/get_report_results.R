
#' Extracting data from the Stics report file
#'
#' @param workspace Stics or JavaStics workspace path containing the
#' report file
#' @param file_name A report file name among "mod_rapport.sti" (default),
#' "mod_rapportA.sti", "mod_rapportP.sti"
#' @param usm_name usm name(s) to filter
#' @param var_list vector of output variables names to filter
#' (optional, see `get_var_info()` to get the names of the variables)
#'
#' @return A data.frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- get_examples_path(file_type = "sti")
#' get_report_results(workspace = path)
#'
#' get_report_results(workspace = path, file_name = "mod_rapportA.sti")
#' }
#'
get_report_results <- function(workspace,
                               file_name = "mod_rapport.sti",
                               usm_name = NULL,
                               var_list = NULL
) {

  files_name <- c("mod_rapport.sti", "mod_rapportA.sti", "mod_rapportP.sti")
  usm_col <- "P_usm"
  usm_patt <- paste0("^", usm_col)

  # Checking file name
  if (! file_name %in% files_name) stop("Unknown report file name:", file_name)

  # Reading file lines
  file_lines <- readLines(file.path(workspace, file_name ))

  # Checking if multiple headers in file are all identical
  h_idx <- grep(pattern = usm_patt, x = file_lines)
  if (length(h_idx)) {
    h <- utils::read.table(text = file_lines[h_idx],
                           sep = ";",
                           fill = TRUE,
                           strip.white = TRUE,
                           na.strings = "",
                           stringsAsFactors = FALSE)

    if(any(is.na(h))) stop("Headers strings are not homogeneous in report file!")

  } else {
    warning("Not any header in report file (no col names in ouput)!")
  }

  # Getting data into a data.frame
  data_idx <- grep(pattern = usm_patt, x = file_lines, invert = TRUE)

  if(!length(data_idx)) stop("Not any data in report file!")

  df <- utils::read.table(text = file_lines[data_idx],
                          sep = ";",
                          strip.white = TRUE,
                          na.strings = "",
                          stringsAsFactors = FALSE)



  # If report header is present
  # otherwise column are named V1 to Vncol
  if (length(h_idx)) {
    col_names <- unlist(h[1,])
    df <- df[,1:length(col_names)]
    names(df) <- col_names
  } else {
    col_names <- names(df)
  }

  # To be homogenous with get_daily_results, get_obs_int
  colnames(df)= var_to_col_names(colnames(df))

  # Filtering usms
  if (!base::is.null(usm_name))  {
    df <- df %>% dplyr::filter(df[[usm_col]] %in% usm_name)
  }

  # Filtering variables
  if (!base::is.null(var_list))  {
    df <- df %>% dplyr::select(c(col_names[1:11], var_to_col_names(var_list)))
  }

  return(df)

}
