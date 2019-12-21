#' Read STICS input meteo file
#'
#' @description Read the meteorology input for STICS ("climat.txt")
#'
#' @param dirpath  Directory path
#' @param filename The meteorology file name (default to \code{climat.txt}).
#'
#' @note The time-related variables are summarised into one POSIXct column named
#'       code{Date}.
#'
#' @return A data.frame of the input meteorological variables used as input for the
#'         STICS model.
#'
#' @seealso \code{\link{get_obs_int}}
#'
#' @importFrom data.table fread
#'
#' @examples
#' library(SticsRFiles)
#' path <- system.file("extdata/txt/V9.0", package = "SticsRFiles")
#' Meteo <- get_climate_txt(path)
#'
#' @export
#'
get_climate_txt= function(dirpath=getwd(), filename="climat.txt"){
  file_path <- file.path(dirpath,filename)
  Met= data.table::fread(file_path, data.table = F)
  colnames(Met)= c("station","year","month","day","julian","ttmin","ttmax",
                   "ttrg","ttetp","ttrr","ttvent","ttpm","ttco2")
  Date= data.frame(Date=as.POSIXct(x = paste(Met$year,Met$month,Met$day, sep="-"),
                                   format = "%Y-%m-%d", tz="UTC"))
  Table_Met= cbind(Date,Met[,-c(grep("year|month|day|julian",colnames(Met)))])
  attr(Table_Met, "file")= file_path
  return(Table_Met)
}
