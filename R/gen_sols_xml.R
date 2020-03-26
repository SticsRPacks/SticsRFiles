#' @title Generate Stics sols xml file from a template or an input file
#' @param sols_out_file file path of the output sols xml file
#' @param sols_nb number of soils to create (optional)
#' @param sols_param a table (df, tibble) containing parameters to use (see details)
#' @param sols_in_file file path to an XML file (optional, if not povided, uses a template from the package corresponding to stics_version)
#' @param stics_version the stics version to use (optional, default to last). Only used if sols_in_file= NULL, see details.
#'
#'
#' @details Please see `get_stics_versions_compat()` for the full list of stics versions that can be used for the
#' argument `stics_version`.
#'  `sols_param` is a `data.frame` with the following format:
#'
#'|Soil_name |     argi|  norg|  calc|   pH| albedo|     q0| epc_1|
#'|:---------|--------:|-----:|-----:|----:|------:|------:|-----:|
#'  |USM_T1    | 20.35000| 0.100|  0.52| 8.23|   0.22|  9.630|    30|
#'  |LF1       | 17.00000| 1.900|  0.00| 6.70|   0.22|  9.360|    30|
#'  |LF2       | 17.00000| 1.800|  0.00| 6.70|   0.22|  9.360|    30|
#'  |LAP       | 22.00000| 2.000|  0.00| 6.50|   0.22|  9.760|    25|
#'  |LAS       | 24.05000| 2.500| 30.00| 8.00|   0.22|  9.928|    30|
#'  |LA0       | 30.00675| 2.300|  0.50| 7.50|   0.22| 10.400|    30|
#'  |LC0       | 22.38750| 2.000| 10.00| 7.90|   0.22|  9.792|    25|
#'  |Vill09    | 25.00000| 0.101|  0.40| 7.90|   0.22| 10.000|    30|
#'  |Vill10    | 14.30000| 0.099|  1.50| 8.20|   0.22|  9.144|    30|
#'  |Vill11    | 11.80000| 0.100|  0.00| 7.30|   0.22|  8.944|    30|
#'  |Vill12    | 14.30000| 0.091|  0.60| 8.30|   0.22|  9.144|    30|
#'  |Vill13    | 16.80000| 0.088|  0.20| 7.80|   0.22|  9.344|    30|
#'  |Vill14    | 15.10000| 0.095|  1.30| 7.90|   0.22|  9.208|    30|
#'
#'
#' The first column gives the soil name, all following
#' columns give the parameter values to put in the sols.xml file for each soil row.
#'
#' The first column name must contain the keyword Soil or soil or SOIL as a prefix to be detected
#' (as shown in the table extract above).
#'
#'
#' @return an invisible xmlDocument object
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' xl_path <- "inputs_stics_example.xlsx"
#' download_usm_xl(xl_name = xl_path)
#' sols_param_df <- read_excel(xl_path, sheet = "Soils")
#' gen_sols_xml(sols_out_file = "sols.xml"),
#' sols_param = sols_param_df)
#'
#' }
#'
#' @export
#'
#'
#'
gen_sols_xml <- function(sols_out_file,
                         sols_nb = NULL,
                         sols_param = NULL,
                         sols_in_file = NULL,
                         stics_version ="last") {

  xml_doc <- NULL

  if (! base::is.null(sols_in_file) ) {
    xml_doc <- xmldocument(sols_in_file)
  }

  xml_doc <- gen_sols_doc(xml_doc = xml_doc,
                          sols_nb = sols_nb,
                          sols_param = sols_param,
                          stics_version = stics_version)

  # checking if out dir exists
  out_path <- dirname(sols_out_file)
  if ( ! dir.exists(out_path) ) {
    stop(paste("The directory does not exist",out_path))
  }

  saveXmlDoc(xml_doc, sols_out_file)

  return(invisible(xml_doc))

}
