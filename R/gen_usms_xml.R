#' @title Generate STICS usms xml file from a template or an input file
#'
#' @param file Path (including name) of the usms file to generate.
#' @param param_df A table (df, tibble) containing the values of the parameters
#' to use (see details)
#' @param template Path of an USM xml file to be used as a template.
#' Optional, if not provided, the function will use a standard template
#' depending on the STICS version.
#' @param stics_version Name of the STICS version. Optional,
#' used if the `file` argument is not provided. In this case the function uses
#' a standard template associated to the STICS version.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of
#' STICS versions that can be used for the argument `stics_version`.
#'
#'  `param_df` is a `data.frame` with the following format:
#'
#'  |usm_name                                  | datedebut| datefin|nomsol |
#'  |:----------------------------------------|---------:|-------:|:------|
#'  |USM_2017_T1_CI                           |       199|     263|USM_T1 |
#'  |USM_2018_T1                              |       264|     570|USM_T1 |
#'  |BIN_CANPC_05_SEC_220-0-0_34K_CANPC05T3_Q |       199|     263|LF1    |
#'  |BIN_AGT_04_IRR_220-0-0_33K_AGT04T2_Q     |       264|     570|LF1    |
#'  |AGA_ARB_13_IRR_220-0-0_37K_ARB13_C       |       199|     263|F1    |
#'  |AGA_ARB_13_SEC_220-0-0_37K_ARB13_C       |       264|     570|LF1    |
#'  |FRA_ARB_11_SEC_220-0-0_38K_E             |       199|     263|LF1    |
#'  |MAG_ARB_09_SEC_220-0-0_38K_E             |       264|     570|LF1    |
#'  |MAG_ARV_12_IRR_220-0-0_36K_ARV12_C       |       199|     263|LF1    |
#'  |MAG_ARV_12_SEC_220-0-0_36K_ARV12_C       |       264|     570|LF1    |
#'  |FRA_ARB_12_SEC_220-0-0_31K_ARB12_C       |       199|     263|LF1    |
#'  |FRA_ARB_13_SEC_220-0-0_37K_ARB13_C       |       264|     570|LF1    |
#'
#'
#' The first column gives the usm name, all following columns give
#' the parameter values to put in the usms.xml file for each usm row.
#'
#' The first column name must contain the keyword Usm or usm or USM
#' as a prefix to be detected (as shown in the table extract above).
#'
#' If not given (the default, `NULL`), the function returns the template as is.
#'
#' @return an invisible xml_document object
#'
#' @examples
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#' usms_param_df <- read_params_table(file = xl_path, sheet_name = "USMs")
#' gen_usms_xml(
#'   file = file.path(tempdir(), "usms.xml"),
#'   param_df = usms_param_df
#' )
#'
#' @export
#'

gen_usms_xml <- function(
  file,
  param_df,
  template = NULL,
  stics_version = "latest"
) {
  xml_doc_tmpl <- NULL

  # If a template file is provided
  if (!base::is.null(template)) {
    xml_doc_tmpl <- xmldocument(template)
  }

  vars <- c("flai_1", "fplt_2", "ftec_2", "flai_2")
  vars_idx <- vars %in% names(param_df)

  # replacing NA or "", with "null", if any vars name in param_df names
  if (!base::is.null(param_df) && any(vars_idx)) {
    for (v in vars[vars_idx]) {
      rep_idx <- is.na(param_df[[v]]) | nchar(param_df[[v]]) == 0
      param_df[[v]][rep_idx] <- "null"
    }
  }

  usms_nb <- nrow(param_df)

  # Common switch function for sols.xml and usms.xml files
  xml_doc <- gen_usms_sols_doc(
    doc_type = "usms",
    xml_doc_tmpl,
    nodes_nb = usms_nb,
    nodes_param = param_df,
    stics_version = stics_version
  )

  # checking if out dir exists
  out_path <- dirname(file)
  if (!dir.exists(out_path)) {
    stop(paste("The directory does not exist: ", out_path))
  }

  # for getting only the xml doc in return
  if (!base::is.null(file)) {
    save_xml_doc(xml_doc, file)
  }

  if (!is.null(xml_doc_tmpl) && inherits(xml_doc_tmpl, "xml_document")) {
    delete(xml_doc_tmpl)
  }

  delete(xml_doc)
}
