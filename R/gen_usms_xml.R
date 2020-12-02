#' @title Generate Stics usms xml file from a template or an input file
#'
#' @param usms_out_file file path of the output usms xml file (optional)
#' @param usms_nb number of usms to create (optional)
#' @param usms_param a table (df, tibble) containing parameters to use (see details)
#' @param usms_in_file file path to an XML file (optional, if not povided, uses a template from the package corresponding to stics_version)
#' @param stics_version the stics version to use (optional, default to last). Only used if usms_in_file= NULL, see details.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of stics versions that can be used for the
#' argument `stics_version`.
#'  `usms_param` is a `data.frame` with the following format:
#'
#'  |usm_name                                  | datedebut| datefin|finit               |nomsol |fstation         |
#'  |:----------------------------------------|---------:|-------:|:-------------------|:------|:----------------|
#'  |USM_2017_T1_CI                           |       199|     263|USM_2017_T1_ini.xml |USM_T1 |climatex_sta.xml |
#'  |USM_2018_T1                              |       264|     570|USM_2017_T1_ini.xml |USM_T1 |climatex_sta.xml |
#'  |BIN_CANPC_05_SEC_220-0-0_34K_CANPC05T3_Q |       199|     263|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |BIN_AGT_04_IRR_220-0-0_33K_AGT04T2_Q     |       264|     570|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |AGA_ARB_13_IRR_220-0-0_37K_ARB13_C       |       199|     263|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |AGA_ARB_13_SEC_220-0-0_37K_ARB13_C       |       264|     570|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |FRA_ARB_11_SEC_220-0-0_38K_E             |       199|     263|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |MAG_ARB_09_SEC_220-0-0_38K_E             |       264|     570|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |MAG_ARV_12_IRR_220-0-0_36K_ARV12_C       |       199|     263|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |MAG_ARV_12_SEC_220-0-0_36K_ARV12_C       |       264|     570|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |FRA_ARB_12_SEC_220-0-0_31K_ARB12_C       |       199|     263|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'  |FRA_ARB_13_SEC_220-0-0_37K_ARB13_C       |       264|     570|Vill11_ini.xml      |LF1    |climatex_sta.xml |
#'
#'
#' The first column gives the usm name, all following
#' columns give the parameter values to put in the usms.xml file for each usm row.
#'
#' The first column name must contain the keyword Usm or usm or USM as a prefix to be detected
#' (as shown in the table extract above).
#'
#'
#' @return an invisible xmlDocument object
#'
#' @examples
#' \dontrun{
#'
#' xl_path <-  download_usm_xl(xl_name = "inputs_stics_example.xlsx")
#' usms_param_df <- read_params_table(file_path = xl_path, sheet_name = "USMs")
#' gen_usms_xml(usms_out_file = "usms.xml", usms_param = usms_param_df)
#'
#' }
#'
#' @export
#'

gen_usms_xml <- function(usms_out_file = NULL,
                         usms_nb = NULL,
                         usms_param = NULL,
                         usms_in_file = NULL,
                         stics_version ="last") {


  xml_doc <- NULL

  if (! base::is.null(usms_in_file) ) {
    xml_doc <- xmldocument(usms_in_file)
  }

  vars <- c("flai_1", "fplt_2","ftec_2", "flai_2")
  vars_idx <- vars %in% names(usms_param)

  # replacing NA with "null", if any vars name in usms_param names
  if (! base::is.null(usms_param) && any(vars_idx)) {
    for (v in vars[vars_idx]) {
      usms_param[[v]][is.na(usms_param[[v]])] <- "null"
    }
  }

  # xml_doc <- gen_usms_doc(xml_doc = xml_doc,
  #                         usms_nb = usms_nb,
  #                         usms_param = usms_param,
  #                         stics_version = stics_version)

  xml_doc <- gen_usms_sols_doc(doc_type = "usms",
                         xml_doc,
                         nodes_nb = usms_nb,
                         nodes_param = usms_param,
                         stics_version = stics_version)

  # checking if out dir exists
  out_path <- dirname(usms_out_file)
  if ( ! dir.exists(out_path) ) {
    stop(paste("The directory does not exist: ",out_path))
  }


  # for getting onlye the xml doc in return
  if (!base::is.null(usms_out_file)) {
    saveXmlDoc(xml_doc, usms_out_file)
  }


  return(invisible(xml_doc))

}
