#' Extract USM years
#'
#' @description Extract USM years ("usms.xml")
#'
#' @param workspace Path of the workspace containing the STICS files to read
#' @param file_name Name of the XML file listing the USMs (default to \code{usms.xml}).
#' @param usm Character vector of USM name(s) for which the years will be extracted (default to all USMs).
#'
#' @return A data.frame of both years for each requested USM.
#'
#' @export
#'
get_years <- function(workspace, file_name = "usms.xml", usm) {
  file_path <- file.path(workspace, file_name)

  # Checking file
  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return()
  }

  requested_usms <- NA
  all_usms <- get_usms_list(file_path)
  if (missing(usm)) {
    requested_usms <- all_usms
  } else {
    stopifnot(usm %in% all_usms)
    requested_usms <- usm
  }

  values <- data.frame(
    usm = requested_usms,
    year1 = NA,
    year2 = NA
  )
  rownames(values) <- values$usm

  for (usm in requested_usms) {
    for (i in 1:2) {
      fclim_id <- paste0("fclim", i)
      fclim <- get_param_xml(
        file = file_path,
        param = fclim_id,
        select = "usm",
        select_value = usm
      )[[1]]
      fclim_path <- file.path(workspace, fclim[[fclim_id]])
      if (!file.exists(fclim_path)) {
        msg <- paste0(
          "file fclim", i, "'", fclim_path, "' does not exist",
          " for USM '", usm, "'"
        )
        warning(msg)
        next
      }
      line1 <- readLines(fclim_path, n = 1)
      tmp <- strsplit(line1, "\\s+")[[1]]
      values[usm, paste0("year", i)] <- as.numeric(tmp[2])
    }
  }

  return(values)
}
