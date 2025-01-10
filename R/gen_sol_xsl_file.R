#' Generating the soil xsl stylesheet for a soil name
#'
#' @param soil_name an usm soil name
#' @param stics_version the STICS files version to use
#'
#' @return conversion success status (TRUE/FALSE)
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::gen_sol_xsl_file("soil_name", "V10" )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
gen_sol_xsl_file <- function(soil_name, stics_version = "latest") {

  xsl_dir <- get_examples_path("xsl", stics_version = stics_version)

  sol_xsl <- file.path(xsl_dir, "sol2txt.xsl")
  sol_xsl_tmpl <- file.path(tempdir(), "sol2txt.xsl")

  if (file.exists(sol_xsl_tmpl)) unlink(sol_xsl_tmpl)

  file_lines <- readLines(sol_xsl)

  # idx of xsl:variable line
  idx <- grep(pattern = "variable", x = file_lines)

  # replace nomsol in it
  file_lines[idx] <- gsub(pattern = "\\?",
                          x = file_lines[idx],
                          replacement = soil_name)

  ret <- try(writeLines(text = file_lines, con = sol_xsl_tmpl))

  if (methods::is(ret, "try-error")) {
    ret <- FALSE
  } else {
    ret <- TRUE
  }

  attr(ret, "path") <- sol_xsl_tmpl

  return(invisible(ret))
}
