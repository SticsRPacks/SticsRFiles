#' @title Generate STICS ini xml file(s) from a template or an input file
#'
#' @param param_df A table (df, tibble) containing the values of the parameters
#' to use (see details)
#' @param file Path of an ini xml file to be used as a template. Optional,
#' if not provided, the function will use a standard template depending on
#' the STICS version (see `stics_version` argument)
#' @param out_dir Path of the directory where to generate the file(s).
#' @param crop_tag identifier for the crop parameters names related to the
#' main crop, or the associated crop if any (example: Crop is used in the
#' param_table example in the details section below)
#' @param stics_version Name of the STICS version.
#' Optional, used if the `file` argument is not provided. In this case
#' the function uses a standard template associated to the STICS version.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of
#' STICS versions that can be used for the
#' argument `stics_version`.
#'
#'  `param_df` is a `data.frame` with the following format:
#'
#'
#'  |Ini_name            | nbplantes|stade0_Crop1 | lai0_Crop1| masec0_Crop1|
#'  |:-------------------|---------:|:------------|----------:|------------:|
#'  |USM_2017_T1_ini.xml |         1|snu          |          0|            0|
#'  |Vill09_ini.xml      |         1|snu          |          0|            0|
#'  |Vill10_ini.xml      |         1|snu          |          0|            0|
#'  |Vill11_ini.xml      |         1|snu          |          0|            0|
#'  |Vill12_ini.xml      |         1|snu          |          0|            0|
#'  |Vill13_ini.xml      |         1|snu          |          0|            0|
#'  |Vill14_ini.xml      |         1|snu          |          0|            0|
#'  |Standard_ini.xml    |         1|snu          |          0|            0|
#'
#'
#' The first column gives the ini file name (to be generated), all following
#' columns give the parameter value to put
#' in the file, and each line denotes a separate ini file
#' (for e.g. several USMs).
#'
#' The first column name must contain the keyword ini or Ini or INI as a prefix
#' to be detected (as shown in the table extract above).
#'
#' If not given (the default, `NULL`), the function returns the template as is.
#'
#' @return None
#'
#' @examples
#' library(readxl)
#'
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#'
#' ini_param_df <- read_excel(xl_path, sheet = "Ini")
#' gen_ini_xml(
#'   out_dir = tempdir(),
#'   param_df = ini_param_df[1:2, ]
#' )
#'
#' @export
#'
gen_ini_xml <- function(
  param_df,
  file = NULL,
  out_dir,
  crop_tag = "Crop",
  stics_version = "latest"
) {
  xml_doc_tmpl <- NULL

  if (!base::is.null(file)) {
    xml_doc_tmpl <- xmldocument(file)
  }

  # Detect the initialization names column
  param_names <- names(param_df)
  col_id <- grep("^ini", tolower(param_names))
  if (!length(col_id)) {
    stop("The column for identifying ini names has not been found !")
  }
  # Initialization names or file names
  ini_col <- param_names[col_id]

  # Generating xml documents for all table lines
  # and removing initialization names col
  xml_docs <- gen_ini_doc(
    xml_doc = xml_doc_tmpl,
    param_table = param_df[, -col_id],
    crop_tag = crop_tag,
    stics_version = stics_version
  )

  if (!is.list(xml_docs) && methods::is(xml_docs, "xml_document")) {
    xml_docs <- list(xml_docs)
  }

  # Finding non NULL elements in xml_docs (i.e. no errors in doc generation)
  out_idx <- unlist(lapply(xml_docs, base::is.null))

  if (any(out_idx)) {
    message(
      "\nErrors have been detected while trying to replace",
      "parameters values in xml documents",
      paste(sum(!out_idx), "files have been generated !"),
      appendLF = TRUE
    )
    # Selecting available documents to produce
    xml_docs <- xml_docs[out_idx]
  }

  # No files will be generated
  if (all(out_idx)) {
    return(invisible())
  }

  # Checking if out_dir exists
  if (!dir.exists(out_dir)) {
    stop("The directory does not exist: ", out_dir)
  }

  # Defining output files paths
  out_name <- param_df[[ini_col]]
  ids <- grepl("_ini.xml$", out_name)
  if (sum(ids) < length(out_name)) {
    out_name[!ids] <- paste0(param_df[[ini_col]][!ids], "_ini.xml")
  }
  ini_out_file <- file.path(out_dir, out_name)

  # Checking dimensions
  if (length(xml_docs) != length(ini_out_file)) {
    stop("Xml output files names must have the same length as table lines ! ")
  }

  # Saving files
  # TODO: vectorize the saveXmlDoc method of the xml_document class
  for (f in seq_along(xml_docs)) {
    save_xml_doc(xml_docs[[f]], ini_out_file[[f]])

    # Finalizing objects
    delete(xml_docs[[f]])
  }

  # Finalizing the template object if provided
  if (!base::is.null(xml_doc_tmpl) && inherits(xml_doc_tmpl, "xml_document")) {
    delete(xml_doc_tmpl)
  }
}
