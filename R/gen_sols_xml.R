#' @title Generate STICS sols xml file from a template or an input file
#'
#' @param file Path (including name) of the sols file to generate.
#' @param param_df A table (df, tibble) containing the values of the parameters
#'  to use (see details)
#' @param template Path of a soil xml file to be used as a template. Optional,
#' if not provided, the function will use a standard template depending on
#' the STICS version.
#' @param stics_version Name of the STICS version. Optional, used if the `file`
#' argument is not provided. In this case the function uses a standard template
#' associated to the STICS version.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of
#' STICS versions that can be used for the argument `stics_version`.
#'
#'  `param_df` is a `data.frame` with the following format:
#'
#'  |Soil_name |     argi|  norg|  calc|   pH| albedo|     q0| epc_1|
#'  |:---------|--------:|-----:|-----:|----:|------:|------:|-----:|
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
#' columns give the parameter values to put in the sols.xml file for each
#' soil row.
#'
#' The first column name must contain the keyword Soil or soil or SOIL as
#' a prefix to be detected (as shown in the table extract above).
#'
#' If not given (the default, `NULL`), the function returns the template as is.
#'
#' @return None
#'
#' @examples
#'
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#'
#' sols_param_df <- read_params_table(file = xl_path, sheet_name = "Soils")
#' gen_sols_xml(
#'   file = file.path(tempdir(), "sols.xml"),
#'   param_df = sols_param_df
#' )
#'
#' @export
#'
gen_sols_xml <- function(
  file,
  param_df,
  template = NULL,
  stics_version = "latest"
) {
  sols_nb <- nrow(param_df)
  xml_doc_tmpl <- NULL

  if (!base::is.null(template)) {
    xml_doc_tmpl <- xmldocument(template)
  }

  xml_doc <- gen_usms_sols_doc(
    doc_type = "sols",
    xml_doc = xml_doc_tmpl,
    nodes_nb = sols_nb,
    nodes_param = param_df,
    stics_version = stics_version
  )

  # checking if out dir exists
  out_path <- dirname(file)
  if (!dir.exists(out_path)) {
    stop(paste("The directory does not exist: ", out_path))
  }

  # saving file
  save_xml_doc(xml_doc, file)

  # finalizing object
  delete(xml_doc)

  if (!base::is.null(xml_doc_tmpl) && inherits(xml_doc_tmpl, "xml_document")) {
    delete(xml_doc_tmpl)
  }
}
