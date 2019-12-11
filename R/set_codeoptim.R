#' @title Set codoptim
#' @description Change value of codeoptim in the new_travail.usm file
#' @param workspace Path of the Stics inputs files of an usm
#' @param value Value of the codeoptim parameter (1 = activating
#' parameters values forcing)
#' @param file_name Name of the file
#'
#'
set_codeoptim <- function(workspace, value = 1, file_name = "new_travail.usm") {


  if (! dir.exists(workspace)) {
    stop(paste(workspace,"directory does not exist !"))
  }

  file_path <- file.path(workspace, file_name)

  if (! file.exists(file_path)) {
    stop(paste(file_path,"does not exist !"))
  }

  lines <- readLines(file_path)

  idx <- grep("codeoptim",lines) + 1
  lines[idx] <- as.character(value)

  writeLines(lines, file_path)


}
