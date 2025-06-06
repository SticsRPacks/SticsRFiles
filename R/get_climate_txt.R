#' Read STICS input meteorology file
#'
#' @description Read the meteorology input for STICS ("climat.txt")
#'
#' @param workspace Path of the workspace containing the STICS
#' climate file to read
#' @param file_name The meteorology file name (default to \code{climat.txt}).
#' @param preserve Logical, `TRUE`` for keeping the STICS columns related
#' to date calculation (year, month, day, julian),
#' or only the Date column as a `POSIXct` otherwise. Default to `TRUE`.
#'
#' @note The time-related variables are summarized into one POSIXct column named
#'       `date`.
#'
#' @return A data.frame of the input meteorological variables used as input
#'         for the STICS model.
#'
#'
#' @examples
#' path <- get_examples_path(file_type = "txt")
#' meteo_data <- get_climate_txt(path)
#'
#' @export
#'
get_climate_txt <- function(
    workspace,
    file_name = "climat.txt",
    preserve = TRUE) {
  file_path <- file.path(workspace, file_name)

  # Checking file
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return()
  }

  meteo_data <- data.table::fread(file_path, data.table = FALSE)
  colnames(meteo_data) <- c(
    "station",
    "year",
    "month",
    "day",
    "julian",
    "ttmin",
    "ttmax",
    "ttrg",
    "ttetp",
    "ttrr",
    "ttvent",
    "ttpm",
    "ttco2"
  )
  date <- data.frame(
    Date = as.POSIXct(
      x = paste(meteo_data$year, meteo_data$month, meteo_data$day, sep = "-"),
      format = "%Y-%m-%d",
      tz = "UTC"
    )
  )

  # For removing original date components columns
  if (!preserve) {
    col_idx <- grep("year|month|day|julian", colnames(meteo_data))
    if (length(col_idx)) meteo_data <- meteo_data[, -col_idx]
  }

  # Adding date to data.frame
  meteo_data <- cbind(date, meteo_data)

  # Adding file path as attribute
  attr(meteo_data, "file") <- file_path

  return(meteo_data)
}
