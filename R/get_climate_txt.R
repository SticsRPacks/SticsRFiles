#' Read STICS input meteo file
#'
#' @description Read the meteorology input for STICS ("climat.txt")
#'
#' @param dirpath  Directory path
#' @param filename The meteorology file name (default to \code{climat.txt}).
#' @param preserve Logical, `TRUE`` for keeping the stics columns related to date (year,
#' month, day, julian), or only keep the date as a `POSIXct` otherwise. Default to `TRUE`.
#'
#' @note The time-related variables are summarised into one POSIXct column named
#'       `Date`.
#'
#' @return A data.frame of the input meteorological variables used as input for the
#'         STICS model.
#'
#' @importFrom data.table fread
#'
#' @examples
#' library(SticsRFiles)
#' path <- get_examples_path( file_type = "txt")
#' Meteo <- get_climate_txt(path)
#'
#' @export
#'
get_climate_txt= function(dirpath=getwd(),
                          filename="climat.txt",
                          preserve = TRUE){

  file_path <- file.path(dirpath,filename)

  # Checking file
  if (! file.exists(file_path)) {
    warning("File does not exist: ",file_path)
    return()
  }

  meteo_data= data.table::fread(file_path, data.table = F)
  colnames(meteo_data)= c("station","year","month","day","julian","ttmin","ttmax",
                          "ttrg","ttetp","ttrr","ttvent","ttpm","ttco2")
  Date= data.frame(Date=as.POSIXct(x = paste(meteo_data$year,meteo_data$month,meteo_data$day, sep="-"),
                                   format = "%Y-%m-%d", tz="UTC"))

  # For removing original date components columns
  if (! preserve ) {
    col_idx <- grep("year|month|day|julian",colnames(meteo_data))
    if (length(col_idx)) meteo_data <- meteo_data[,-col_idx]
  }

  # Adding Date to data.frame
  meteo_data= cbind(Date, meteo_data)

  # Adding file path as attribute
  attr(meteo_data, "file")= file_path

  return(meteo_data)
}
