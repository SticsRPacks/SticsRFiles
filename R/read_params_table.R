#' Getting parameters data from tables files (Excel sheet, csv)
#'
#' @param file_path Excel or csv file path
#' @param sheet_name Name of an Excel sheet (useless for csv files)
#'
#' @return A tibble of parameters
#' @export
#'
#' @examples
#'
#' usm_xl_file <-  download_usm_xl(xl_name = "inputs_stics_example.xlsx")
#' xl_param <- read_excel(usm_xl_file, sheet = "USMs")
#'
#' usm_csv_file <- download_usm_csv(csv_name = "inputs_stics_example_USMs.csv")
#' read_params_table(file_path = usm_csv_file)
#'
read_params_table <- function(file_path, sheet_name = NULL) {

  # files extension list
  files_ext_lst <- c("csv", "xls", "xlsx")

  # Getting file extension, and checking its validity
  file_ext <- tools::file_ext(file_path)
  if (isFALSE(file_ext %in% files_ext_lst)) {
    stop(paste0('"',file_ext,'"'),": is not a valid extension")
  }

  # Detecting if the sheet exists in XL file
  # dfault value for csv file
  sheet_exists <- TRUE
  if (isFALSE("csv" == file_ext))  {
    xl_sheets <- readxl::excel_sheets(file_path)
    sheet_exists <- sheet_name %in% xl_sheets
  }
  # If the sheet name is not provided
  if (!length(sheet_exists)) sheet_exists <- FALSE


  # Unknown sheet in XL file
  if (isFALSE(sheet_exists)) {
    warning(paste(sheet_name,
                  "unknown or not set sheet name!\n",
                  "Check in list:\n",
                  paste(xl_sheets, collapse = ", ")))
    return()
  }

  # Reading file according to its format
  switch(file_ext,
         csv={
           out_table <- utils::read.csv2(file = file_path,
                                  header = TRUE,
                                  sep = ";",
                                  stringsAsFactors = FALSE)
         },
         {
           out_table <- readxl::read_excel(usm_xl_file, sheet = sheet_name)
         }
  )

  # Casting logical columns to character type
  out_table %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.logical, as.character) -> out_table

  # Replacing NA with empty string
  out_table[is.na(out_table)] <- ""

  out_table

}
