#' @title Generate Stics tec xml file(s) from a template or an input file
#'
#' @param param_df A table (df, tibble) containing the values of the parameters to use (see details)
#' @param file Path of a tec xml file to be used as a template. Optional, if not provided, the function will use a standard template depending on the stics version.
#' @param out_dir Path of the directory where to generate the file(s).
#' @param stics_version Name of the Stics version. Optional, used if the `file` argument is not provided to use a standard template depending on the stics version. By default the latest version returned by `get_stics_versions_compat()` is used.
#' @param na_values value to use as missing value in param_table (optional, default : NA)
#' @param param_table `r lifecycle::badge("deprecated")` `param_table` is no
#'   longer supported, use `param_df` instead.
#' @param tec_in_file `r lifecycle::badge("deprecated")` `tec_in_file` is no
#'   longer supported, use `file` instead.
#' @param out_path `r lifecycle::badge("deprecated")` `out_path` is no
#'   longer supported, use `out_dir` instead.
# @param dict List of correspondance between given parameter names and internal names.
#'
#' @details
#'
#'  `param_table` is a `data.frame` with the following format:
#'
#' |Tec_name                                         | julres_1| coderes_1| qres_1| Crespc_1| CsurNres_1|
#' |:------------------------------------------------|--------:|---------:|------:|--------:|----------:|
#' |USM_2017_T1_CI_tec.xml                           |      NA |         1|      9|       42|         90|
#' |BIN_CANPC_05_SEC_220-0-0_34K_CANPC05T3_Q_tec.xml |      110|         1|      9|       42|         90|
#' |BIN_AGT_04_IRR_220-0-0_33K_AGT04T2_Q_tec.xml     |       73|         1|      9|       42|         90|
#' |AGA_ARB_13_IRR_220-0-0_37K_ARB13_C_tec.xml       |       82|         1|      9|       42|         90|
#' |AGA_ARB_13_SEC_220-0-0_37K_ARB13_C_tec.xml       |       82|         1|      9|       42|         90|
#' |FRA_ARB_11_SEC_220-0-0_38K_E_tec.xml             |       70|         1|      9|       42|         90|
#' |MAG_ARB_09_SEC_220-0-0_38K_E_tec.xml             |       81|         1|      9|       42|         90|
#' |MAG_ARV_12_IRR_220-0-0_36K_ARV12_C_tec.xml       |      100|         1|      9|       42|         90|
#' |MAG_ARV_12_SEC_220-0-0_36K_ARV12_C_tec.xml       |      100|         1|      9|       42|         90|
#' |FRA_ARB_12_SEC_220-0-0_31K_ARB12_C_tec.xml       |       92|         1|      9|       42|         90|
#' |FRA_ARB_13_SEC_220-0-0_37K_ARB13_C_tec.xml       |       82|         1|      9|       42|         90|
#'
#' The first column gives the tec file name (to be generated), all following columns give the parameter value to put
#' in the file, and each line denotes a separate tec file (for e.g. several USMs).
#'
#' The first column name must contain the keyword tec or Tec or TEC as a prefix to be detected
#' (as shown in the table extract above).
#'
#' @return an invisible xmlDocument object or a list of
#'
#' @examples
#' \dontrun{
#'
#' xl_path <-  download_usm_xl(file = "inputs_stics_example.xlsx")
#' tec_param_df <- read_params_table(file_path = xl_path, sheet_name = "Tec")
#' gen_tec_xml(out_dir = "/path/to/dest/dir", param_df = tec_param_df)
#'
#'}
#'
#' @export
#'
# TODO: refactor with gen_sta_file, gen_ini_file : same code
gen_tec_xml <- function(param_df = NULL,
                        file = NULL,
                        #tec_names = NULL,
                        out_dir = getwd(),
                        stics_version = "latest",
                        na_values = NA,#) { #,  #dict = NULL) {
                        param_table = lifecycle::deprecated(),
                        tec_in_file = lifecycle::deprecated(),
                        out_path = lifecycle::deprecated()) {

    if (lifecycle::is_present(param_table)) {
      lifecycle::deprecate_warn("0.5.0", "gen_tec_xml(param_table)", "gen_tec_xml(param_df)")
    } else {
      param_table <- param_df # to remove when we update inside the function
    }
    if (lifecycle::is_present(tec_in_file)) {
      lifecycle::deprecate_warn("0.5.0", "gen_tec_xml(tec_in_file)", "gen_tec_xml(file)")
    } else {
      tec_in_file <- file # to remove when we update inside the function
    }
    if (lifecycle::is_present(out_path)) {
      lifecycle::deprecate_warn("0.5.0", "gen_tec_xml(out_path)", "gen_tec_xml(out_dir)")
    } else {
      out_path <- out_dir # to remove when we update inside the function
    }



  xml_doc <- NULL

  if (! base::is.null(tec_in_file) ) {
    xml_doc <- xmldocument(tec_in_file)
  }


  # detecting tec names column
  param_names <- names(param_table)
  col_id <- grep("^tec",tolower(param_names))
  if (! length(col_id)) {
    stop("The column for identifying tec names has not been found !")
  }
  tec_col <- param_names[ col_id  ]

  # Pending -------------------------------------------------------------
  # Filtering on tec_names vector arg
  # if (!is.null(tec_names)) {
  #   param_table <- param_table[param_table[[tec_col]] %in% tec_names, ]
  # }
  #----------------------------------------------------------------------


  # Removing for the moment the dict argument
  xml_docs <- gen_tec_doc(xml_doc = xml_doc,
                          param_table = param_table[ , - col_id],
                          stics_version = stics_version,
                          na_values = na_values) #,
  #dict = dict)


  if ( class(xml_docs) == "xmlDocument") {
    xml_docs <- list(xml_docs)
  }


  # Finding non NULL elements in xml_docs (i.e. no errors in doc generation)
  out_idx <- unlist(lapply(xml_docs, base::is.null))

  if (any(out_idx)) {
    cat("\n")
    cat("Errors have been detected while trying to replace parameters values in xml documents\n")
    cat(paste(sum(!out_idx), "files have been generated !\n"))
    # selecting available documents to produce
    xml_docs <- xml_docs[out_idx]
  }

  # No files will be generated
  if (all(out_idx)) {
    return(invisible())
  }



  # checking if out_path exists
  if ( ! dir.exists(out_path) ) {
    stop(paste("The directory does not exist",out_path ))
  }

  # defining output files paths
  out_name <- param_table[[tec_col]]
  ids <- grepl("_tec.xml$",out_name)
  if ( sum(ids) < length(out_name) ) {
    out_name[!ids] <- paste0(param_table[[tec_col]][!ids],"_tec.xml")
  }
  tec_out_file <- file.path(out_path,out_name)



  # checking dimensions
  if ( ! length(xml_docs) == length(tec_out_file) ) {
    stop("Xml output files names must have the same length as table lines ! ")
  }

  # selecting output files names
  tec_out_file <- tec_out_file[! out_idx]

  # saving files
  # TODO: vectorize the saveXmlDoc method of the xmlDocument class
  for (f in 1:length(xml_docs)) {
    saveXmlDoc(xml_docs[[f]], tec_out_file[[f]])
  }

  return(invisible(xml_docs))

}
