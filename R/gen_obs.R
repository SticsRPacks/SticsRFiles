#' Write STICS obs data from data.frame
#'
#' @description Write STICS obs data from data.frame/tibble
#'
#' @param obs_table data.frame to write
#' @param file_path Path to the file to write to
#'
#' @examples
#'
#' \dontrun{
#' # Getting observations data
#' xl_path <- download_usm_xl(xl_name = "inputs_stics_example.xlsx")
#'
#' # Loading and filtering data for usm "USM_2017_T1_CI"
#' obs_df <- read_params_table(file_path = xl_path, sheet_name = "Obs") %>%
#' dplyr::filter(usm_name %in% "USM_2017_T1_CI")
#'
#' # Generating the csv file
#' gen_obs(obs_df, "USM_2017_T1_CI.obs")
#'
#' }
#'
#' @return A logical value if the file generation succeeded (T) or not (F)
#'
#' @keywords internal
#'
gen_obs= function(obs_table, file_path){


  # Checking file path
  dir_name <- dirname(file_path)
  if (! dir.exists(dir_name)) {
    warning(paste("Directory does not exist:",dir_name))
    return(invisible(FALSE))
  }

  # Removing unwanted columns !
  date_plt_idx <- grep("date|plant|usm",tolower(colnames(obs_table)))
  if ( length(date_plt_idx)) {
    obs_table= obs_table[,-date_plt_idx]
  }

  # Checking date columns & variables columns
  patt_str <- "^ian$|^mo$|^jo$|^jul$"
  obs_var_df <- obs_table[,-grep(patt_str, colnames(obs_table))]
  obs_date_df <- obs_table[,grep(patt_str, colnames(obs_table))]

  if (! dim(obs_var_df)[2] || dim(obs_date_df)[2] < 4){
    warning("Missing columns for dates, or no observation variables values to write !")
    return(invisible(FALSE))
  }

  # Ordering date components columns in a data.frame
  obs_table= data.frame(obs_date_df, obs_var_df)

  # TODO: see what is the purpose of _sd ending tag !
  # Linked to associated plants ???
  # colnames(obs_table)= gsub("_n","(n)",colnames(obs_table))
  # colnames(obs_table)[grep("_[0-9]$",colnames(obs_table))]=
  #   gsub("$",")",gsub("_","(",colnames(obs_table)[grep("_[0-9]$",colnames(obs_table))]))
  # colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))]=
  #   gsub("_sd$",")_sd",gsub("_","(",colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))]))
  #
  # splitted_sd= strsplit(colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))],"_")
  # colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))]=
  #   lapply(splitted_sd, function(z){
  #     z= paste(c(paste(z[1:2], collapse = "("),z[3]), collapse = ")_")
  #   })

  # Back to Stics variables names syntax !
  colnames(obs_table) <- col_names_to_var(colnames(obs_table))

  ret <- try(utils::write.table(obs_table,file_path,sep=";",na="-999.99",row.names= F, quote=F))

  # Checking if any error writing the file
  if (methods::is(ret, "try-error")) {
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}
