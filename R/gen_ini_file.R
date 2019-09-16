#' @title Generate from a template or an input file a Stics ini xml file
#' @param ini_out_file file name of the output ini xml file
#'
#' @param param_table a table (df, tibble) containing parameters to use
#' @param ini_in_file file path for an input ini xml file
#' @param out_path path for the ini output file
#' @param crop_tag identifier for the crop parameters names
#' @param stics_version the stics files version to use
#'
#' @return an invisible xmlDocument object or a list of
#'
#' @export
#'
# TODO: refactor with gen_sta_file, gen_tec_file : same code
gen_ini_file <- function(ini_out_file = NULL, param_table = NULL,
                         ini_in_file = NULL,
                         out_path = getwd(),
                         crop_tag = "crop",
                         stics_version ="last") {



  xml_doc <- NULL

  if (! is.null(ini_in_file) ) {
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


  # defining output files paths
  if (is.null(ini_out_file)) {
    out_name <- param_table[[ini_col]]
    ids <- grepl("_ini.xml$",out_name)
    if ( sum(ids) < length(out_name) ) {
      out_name[ids] <- paste0(param_table[[ini_col]][ids],"_ini.xml")
    }
    ini_out_file <- file.path(out_path,out_name)
  } else {
    # file names generation if multiple files

    ini_out_file <- file.path(out_path,ini_out_file)
    if ( length(xml_docs) > 1) {
      ini_out_file <- paste(gsub(x = ini_out_file, replacement = "", pattern = "_ini.xml$"),
                            1:length(xml_docs), sep = "_","ini.xml" )
    }
  }

  # checking dimensions
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
