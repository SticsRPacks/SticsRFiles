#' @title Generate Stics usms xml file from a template or an input file
#'
#' @param file Path (including name) of the usms file to generate. Optional, set to `file.path(getwd(), "usms.xml")`by default.
#' @param param_df A table (df, tibble) containing the values of the parameters to use (see details)
#' @param template Path of an USM xml file to be used as a template. Optional, if not provided, the function will use a standard template depending on the stics version.
#' @param stics_version Name of the Stics version. Optional, used if the `file` argument is not provided. In this case the function uses a standard template associated to the stics version.
#' @param usms_out_file `r lifecycle::badge("deprecated")` `usms_out_file` is no
#'   longer supported, use `file` instead.
#' @param usms_nb `r lifecycle::badge("deprecated")` `usms_nb` is no
#'   longer supported, use `NA` instead.
#' @param usms_param `r lifecycle::badge("deprecated")` `usms_param` is no
#'   longer supported, use `param_df` instead.
#' @param usms_in_file `r lifecycle::badge("deprecated")` `usms_in_file` is no
#'   longer supported, use `template` instead.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of stics versions that can be used for the
#' argument `stics_version`.
#'
#'  `param_df` is a `data.frame` with the following format:
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
#' If not given (the default, `NULL`), the function returns the template as is.
#'
#' @return an invisible xmlDocument object
#'
#' @examples
#' \dontrun{
#'
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#' usms_param_df <- read_params_table(file = xl_path, sheet_name = "USMs")
#' gen_usms_xml(usms_out_file = "usms.xml", usms_param = usms_param_df)
#' }
#'
#' @export
#'

gen_usms_xml <- function(file = file.path(getwd(), "usms.xml"),
                         param_df = NULL,
                         template = NULL,
                         stics_version = "latest",
                         usms_out_file = lifecycle::deprecated(),
                         usms_nb = lifecycle::deprecated(),
                         usms_param = lifecycle::deprecated(),
                         usms_in_file = lifecycle::deprecated()) {
  if (lifecycle::is_present(usms_out_file)) {
    lifecycle::deprecate_warn("0.5.0", "gen_usms_xml(usms_out_file)", "gen_usms_xml(file)")
  } else {
    usms_out_file <- file # to remove when we update inside the function
  }
  if (lifecycle::is_present(usms_param)) {
    lifecycle::deprecate_warn("0.5.0", "gen_usms_xml(usms_param)", "gen_usms_xml(param_df)")
  } else {
    usms_param <- param_df # to remove when we update inside the function
  }
  if (lifecycle::is_present(usms_in_file)) {
    lifecycle::deprecate_warn("0.5.0", "gen_usms_xml(usms_in_file)", "gen_usms_xml(template)")
  } else {
    usms_in_file <- template # to remove when we update inside the function
  }
  if (lifecycle::is_present(usms_nb)) {
    lifecycle::deprecate_warn("0.5.0",
      "gen_usms_xml(usms_nb)",
      details = "It is now directly computed in the function."
    )
  } else {
    usms_nb <- nrow(param_df) # to remove when we update inside the function
  }

  xml_doc <- NULL

  # Fix : default output file path if not provided
  if (base::is.null(usms_out_file)) {
    usms_out_file <- file.path(getwd(), "usms.xml")
  }

  # If a template file is provided
  if (!base::is.null(usms_in_file)) {
    xml_doc <- xmldocument(usms_in_file)
  }

  vars <- c("flai_1", "fplt_2", "ftec_2", "flai_2")
  vars_idx <- vars %in% names(usms_param)

  # replacing NA or "", with "null", if any vars name in usms_param names
  if (!base::is.null(usms_param) && any(vars_idx)) {
    for (v in vars[vars_idx]) {
      rep_idx <- is.na(usms_param[[v]]) | nchar(usms_param[[v]]) == 0
      usms_param[[v]][rep_idx] <- "null"
    }
  }

  # Common switch function for sols.xml and usms.xml files
  xml_doc <- gen_usms_sols_doc(
    doc_type = "usms",
    xml_doc,
    nodes_nb = usms_nb,
    nodes_param = usms_param,
    stics_version = stics_version
  )

  # hecking if out dir exists
  out_path <- dirname(usms_out_file)
  if (!dir.exists(out_path)) {
    stop(paste("The directory does not exist: ", out_path))
  }


  # for getting onlye the xml doc in return
  if (!base::is.null(usms_out_file)) {
    saveXmlDoc(xml_doc, usms_out_file)
  }


  return(invisible(xml_doc))
}
