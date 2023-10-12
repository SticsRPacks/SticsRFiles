#' Generating the soil xsl stylesheet for a soil name
#'
#' @param workspace Path of a JavaSTICS workspace
#' (i.e. containing the STICS XML input files)
#' @param usm an usm name
#' @param stics_version the STICS files version to use
#'
#' @return conversion success status (TRUE/FALSE)
#'
#' @examples
#' \dontrun{
#' SticsRFiles:::gen_sol_xsl_file("path/to/workspace", "usm_name", "V10" )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
gen_sol_xsl_file <- function(workspace, usm, stics_version = "latest") {

  # getting soil name
  soil_name <- get_param_xml(file = file.path(workspace, "usms.xml"),
                             param = "nomsol",
                             select = "usm",
                             select_value = usm)$usms.xml$nomsol

  xsl_dir <- get_examples_path("xsl", stics_version = stics_version)

  sol_xsl <- file.path(xsl_dir, "sol2txt.xsl")
  sol_xsl_tmpl <- file.path(xsl_dir, "sol2txt.xsl.tmpl")

  if(!file.exists(sol_xsl_tmpl)) {
    file.copy(sol_xsl, sol_xsl_tmpl)
  }

  file_lines <- readLines(sol_xsl_tmpl)

  # idx of xsl:variable line
  idx <- grep(pattern = "variable", x = file_lines)

  # replace nomsol in it
  file_lines[idx] <- gsub(pattern = "\\?",
                          x = file_lines[idx],
                          replacement = soil_name)

  ret <- try(writeLines(text = file_lines, con = sol_xsl))

  if (methods::is(ret, "try-error")) {
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}
