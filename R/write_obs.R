#' Write STICS obs data from data.frame
#'
#' @description Write STICS obs data from data.frame
#'
#' @param x       Data.frame to write
#' @param path Path to the file to write to
#'
#' @examples
#'\dontrun{
#' write_obs(obs, "wheat.obs")
#'}
#' @export
#'
write_obs= function(x,path){
  x= x[,-grep("Date|Plant",colnames(x))]
  x= data.frame(x[,grep("ian|mo|jo|jul",colnames(x))],
                x[,-grep("ian|mo|jo|jul",colnames(x))])

  colnames(x)= gsub("_n","(n)",colnames(x))
  colnames(x)[grep("_[0-9]$",colnames(x))]=
    gsub("$",")",gsub("_","(",colnames(x)[grep("_[0-9]$",colnames(x))]))
  colnames(x)[grep("_[0-9]_sd$",colnames(x))]=
    gsub("_sd$",")_sd",gsub("_","(",colnames(x)[grep("_[0-9]_sd$",colnames(x))]))

  splitted_sd= strsplit(colnames(x)[grep("_[0-9]_sd$",colnames(x))],"_")
  colnames(x)[grep("_[0-9]_sd$",colnames(x))]=
    lapply(splitted_sd, function(z){
      z= paste(c(paste(z[1:2], collapse = "("),z[3]), collapse = ")_")
    })

  utils::write.table(x,path,sep=";",na="-999.99",row.names= F, quote=F)
}
