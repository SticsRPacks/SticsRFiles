#' Write STICS obs data from data.frame
#'
#' @description Write STICS obs data from data.frame
#'
#' @param obs_table data.frame to write
#' @param path Path to the file to write to
#'
#' @examples
#'\dontrun{
#' xl_path <- file.path(system.file(package="SticsRFiles","extdata/xl/inputs_stics_example.xlsx")
#' obs_df <- read_excel(xl_path, sheet = "Obs")
#' gen_obs(obs_df, "USM_2017_T1_CI.obs")
#'}
#'
#' @export
#'
gen_obs= function(obs_table,path){
  # obs_table= obs_table[,-grep("Date|Plant",colnames(obs_table))]
  obs_table= data.frame(obs_table[,grep("ian|mo|jo|jul",colnames(obs_table))],
                obs_table[,-grep("ian|mo|jo|jul",colnames(obs_table))])

  colnames(obs_table)= gsub("_n","(n)",colnames(obs_table))
  colnames(obs_table)[grep("_[0-9]$",colnames(obs_table))]=
    gsub("$",")",gsub("_","(",colnames(obs_table)[grep("_[0-9]$",colnames(obs_table))]))
  colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))]=
    gsub("_sd$",")_sd",gsub("_","(",colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))]))

  splitted_sd= strsplit(colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))],"_")
  colnames(obs_table)[grep("_[0-9]_sd$",colnames(obs_table))]=
    lapply(splitted_sd, function(z){
      z= paste(c(paste(z[1:2], collapse = "("),z[3]), collapse = ")_")
    })

  utils::write.table(obs_table,path,sep=";",na="-999.99",row.names= F, quote=F)
}
