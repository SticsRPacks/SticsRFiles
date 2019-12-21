#' @title Generate from a template  or an input file a Stics sta xml file
#' @param sta_out_file file name of the output sta xml file
#' @param param_table a table (df, tibble) containing parameters to use
#' @param sta_in_file file path for an input sta xml file
#' @param out_path path for the sta output file
#' @param stics_version the stics files version to use
#'
#' @return an invisible xmlDocument object or a list of
#'
#' @export
#'
# TODO: refactor with gen_tec_file, gen_ini_file : same code
gen_sta_xml <- function(sta_out_file = NULL, param_table = NULL,
                         sta_in_file = NULL,
                         out_path = getwd(),
                         stics_version ="last") {

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


  xml_docs <- gen_sta_doc(xml_doc_object = xml_doc,
                         param_table = param_table[ , - col_id],
                         stics_version = stics_version)


  if ( class(xml_docs) == "xmlDocument") {
    xml_docs <- list(xml_docs)
  }


  # defining output files paths
  if (base::is.null(sta_out_file)) {
    out_name <- param_table[[sta_col]]
    ids <- grepl("_sta.xml$",out_name)
    if (sum(ids) < length(out_name)) {
      out_name <- paste0(param_table[[sta_col]][ids],"_sta.xml")
    }
    sta_out_file <- file.path(out_path,out_name)
  } else {
    sta_out_file <- file.path(out_path,sta_out_file)
    if ( length(xml_docs) > 1) {
      sta_out_file <- paste(gsub(x = sta_out_file, replacement = "", pattern = "_sta.xml$"),
                            1:length(xml_docs), sep = "_","sta.xml" )
    }
  }

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
