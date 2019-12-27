#' @title Generate from a template  or an input file a Stics tec xml file
#' @param tec_out_file file name of the output tec xml file
#' @param param_table a table (df, tibble) containing parameters to use
#' @param tec_in_file file path for an input tec xml file
#' @param out_path path for the ini output file
#' @param stics_version the stics files version to use
#' @param dict List of correspondance between given parameter names and internal names.
#'
#' @return an invisible xmlDocument object or a list of
#'
#' @examples
#' \dontrun{
#' copy_mailing_example(xl_name = "inputs_stics_example.xlsx", dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir","inputs_stics_example.xlsx")
#' tec_param_df <- read_excel(xl_path, sheet = "Tec")
#' gen_tec_xml(out_path = file.path("/path/to/dest/dir","tec"),
#' param_table = tec_param_df)
#'}
#'
#' @export
#'
# TODO: refactor with gen_sta_file, gen_ini_file : same code
gen_tec_xml <- function(tec_out_file = NULL, param_table = NULL,
                         tec_in_file = NULL,
                         out_path = getwd(),
                         stics_version = "last",
                         dict = NULL) {



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


  xml_docs <- gen_tec_doc(xml_doc_object = xml_doc,
                          param_table = param_table[ , - col_id],
                          stics_version = stics_version,
                          dict = dict)


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
  if (base::is.null(tec_out_file)) {
    out_name <- param_table[[tec_col]]
    ids <- grepl("_tec.xml$",out_name)
    if ( sum(ids) < length(out_name) ) {
      out_name[ids] <- paste0(param_table[[tec_col]][ids],"_tec.xml")
    }
    tec_out_file <- file.path(out_path,out_name)
  } else {
    # file names generation if multiple files

    tec_out_file <- file.path(out_path,tec_out_file)
    if ( length(xml_docs) > 1) {
      tec_out_file <- paste(gsub(x = tec_out_file, replacement = "", pattern = "_tec.xml$"),
                            1:length(xml_docs), sep = "_","tec.xml" )
    }
  }



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
