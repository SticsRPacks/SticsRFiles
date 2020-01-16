#' Download example USMs
#'
#' @description Download locally the example data from the [data repository](https://github.com/SticsRPacks/data)
#' in the SticsRPacks organisation.
#'
#' @param dir The directory where to download the data
#' @return Download the data and return the path to the folder where the data was downloaded
#' @export
#'
#' @examples
#' \dontrun{
#' download_data()
#' }
download_data= function(dir= NULL){
  if(is.null(dir)){
    dir= tempdir()
  }
  data_dir= normalizePath(dir, winslash = "/", mustWork = FALSE)
  data_dir_zip= normalizePath(file.path(data_dir,"master.zip"), winslash = "/", mustWork = FALSE)
  utils::download.file("https://github.com/SticsRPacks/data/archive/master.zip", data_dir_zip)
  utils::unzip(data_dir_zip, exdir = data_dir)
  unlink(data_dir_zip)
  normalizePath(file.path(data_dir,utils::unzip(data_dir_zip,list = TRUE)[1,1]), winslash = "/")
}
