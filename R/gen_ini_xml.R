#' @title Generate Stics ini xml file(s) from a template or an input file
#'
#' @param param_table a table (df, tibble) containing parameters to use (see details)
#' @param ini_in_file file path to an XML file (optional, if not povided, uses a template from the package corresponding to stics_version)
#' @param out_path path to an optional folder where to write the output file(s)
#' @param crop_tag identifier for the crop parameters names related to the main crop, or the associated crop if any
#' (example: Crop is used in the param_table example in the details section below)
#' @param stics_version the stics version to use (optional, default to last). Only used if ini_in_file= NULL, see details.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of stics versions that can be used for the
#' argument `stics_version`.
#'  `param_table` is a `data.frame` with the following format:
#'
#'
#'  |Ini_name            | nbplantes|stade0_Crop1 | lai0_Crop1| masec0_Crop1| QNplante0_Crop1|
#'  |:-------------------|---------:|:------------|----------:|------------:|---------------:|
#'  |USM_2017_T1_ini.xml |         1|snu          |          0|            0|               0|
#'  |Vill09_ini.xml      |         1|snu          |          0|            0|               0|
#'  |Vill10_ini.xml      |         1|snu          |          0|            0|               0|
#'  |Vill11_ini.xml      |         1|snu          |          0|            0|               0|
#'  |Vill12_ini.xml      |         1|snu          |          0|            0|               0|
#'  |Vill13_ini.xml      |         1|snu          |          0|            0|               0|
#'  |Vill14_ini.xml      |         1|snu          |          0|            0|               0|
#'  |Standard_ini.xml    |         1|snu          |          0|            0|               0|
#'
#'
#' The first column gives the ini file name (to be generated), all following columns give the parameter value to put
#' in the file, and each line denotes a separate ini file (for e.g. several USMs).
#'
#' The first column name must contain the keyword ini or Ini or INI as a prefix to be detected
#' (as shown in the table extract above).
#'
#'
#' @return an invisible xmlDocument object or a list of
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' xl_path <- "inputs_stics_example.xlsx"
#' download_usm_xl(xl_name = xl_path)
#' ini_param_df <- read_excel(xl_path, sheet = "Ini")
#' gen_ini_xml(out_path = "/path/to/dest/dir",
#' param_table = ini_param_df)
#' }
#'
#' @export
#'
# TODO: refactor with gen_sta_file, gen_tec_file : same code
gen_ini_xml <- function(param_table = NULL,
                        ini_in_file = NULL,
                        out_path = getwd(),
                        crop_tag = "Crop",
                        stics_version ="last") {

  xml_doc <- NULL

  if (! base::is.null(ini_in_file) ) {
    xml_doc <- xmldocument(ini_in_file)
  }

  # detect the ini names column
  param_names <- names(param_table)
  col_id <- grep("^ini",tolower(param_names))
  if (! length(col_id)) {
    stop("The column for identifying ini names has not been found !")
  }
  # ini names or file names
  ini_col <- param_names[ col_id ]

  # generating xml documents for all table lines
  # removing ini names col
  xml_docs <- gen_ini_doc(xml_doc = xml_doc,
                          param_table = param_table[ , - col_id],
                          crop_tag = crop_tag,
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


  # Checking if out_path exists
  if ( ! dir.exists(out_path) ) {
    stop(paste("The directory does not exist",out_path ))
  }

  # Defining output files paths
  out_name <- param_table[[ini_col]]
  ids <- grepl("_ini.xml$",out_name)
  if ( sum(ids) < length(out_name) ) {
    out_name[!ids] <- paste0(param_table[[ini_col]][!ids],"_ini.xml")
  }
  ini_out_file <- file.path(out_path,out_name)

  # Checking dimensions
  if ( ! length(xml_docs) == length(ini_out_file) ) {
    stop("Xml output files names must have the same length as table lines ! ")
  }

  # saving files
  # TODO: vectorize the saveXmlDoc method of the xmlDocument class
  for (f in 1:length(xml_docs)) {
    saveXmlDoc(xml_docs[[f]], ini_out_file[[f]])
  }

  return(invisible(xml_docs))

}
