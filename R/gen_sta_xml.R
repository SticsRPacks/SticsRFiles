#' @title Generate Stics sta xml file(s) from a template or an input file
#'
#' @param param_table a table (df, tibble) containing parameters to use (see details)
#' @param sta_in_file file path to an XML file (optional, if not povided, uses a template from the package corresponding to stics_version)
#' @param out_path path to an optional folder where to write the output file(s)
#' @param stics_version the stics version to use (optional, default to latest). Only used if sta_in_file= NULL, see details.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of stics versions that can be used for the
#' argument `stics_version`.
#'
#'  `param_table` is a `data.frame` with the following format:
#'
#'  |Sta_name         |  zr| NH3ref| latitude| patm| aclim|
#'  |:----------------|---:|------:|--------:|----:|-----:|
#'  |climatex_sta.xml | 2.5|      0|       49| 1000|    20|
#'  |climatex2_sta.xml | 2.8|      0|       49| 1000|    20|
#'  |climatex3_sta.xml | 2.2|      0|       49| 1000|    20|
#'
#' The first column gives the sta file name (to be generated), all following columns give the parameter value to put
#' in the file, and each line denotes a separate sta file (for e.g. several USMs).
#'
#' The first column name must contain the keyword sta or Sta or STA as a prefix to be detected
#' (as shown in the table extract above).
#'
#' @return an invisible xmlDocument object or a list of
#'
#' @examples
#' \dontrun{
#'
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#' sta_param_df <- read_params_table(file_path = xl_path, sheet_name = "Station")
#' gen_sta_xml(out_path = "/path/to/dest/dir", param_table = sta_param_df)
#'
#'}
#'
#' @export
#'
# TODO: refactor with gen_tec_file, gen_ini_file : same code
gen_sta_xml <- function(param_table = NULL,
                        sta_in_file = NULL,
                        out_path = getwd(),
                        stics_version ="latest") {

  xml_doc <- NULL

  if (! base::is.null(sta_in_file) ) {
    xml_doc <- xmldocument(sta_in_file)
  }


  # detecting sta names column
  param_names <- names(param_table)
  col_id <- grep("^sta",tolower(param_names))
  if (! length(col_id)) {
    stop("The column for identifying sta names has not been found !")
  }
  sta_col <- param_names[ col_id  ]


  xml_docs <- gen_sta_doc(xml_doc = xml_doc,
                          param_table = param_table[ , - col_id],
                          stics_version = stics_version)


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
  out_name <- param_table[[sta_col]]
  ids <- grepl("_sta.xml$",out_name)
  if (sum(ids) < length(out_name)) {
    out_name[!ids] <- paste0(param_table[[sta_col]][!ids],"_sta.xml")
  }
  sta_out_file <- file.path(out_path,out_name)

  # checking dimensions
  if ( ! length(xml_docs) == length(sta_out_file) ) {
    stop("Xml output files names must have the same length as table lines ! ")
  }

  # saving files
  # TODO: vectorize the saveXmlDoc method of the xmlDocument class
  for (f in 1:length(xml_docs)) {
    saveXmlDoc(xml_docs[[f]], sta_out_file[[f]])
  }

  return(invisible(xml_docs))

}
