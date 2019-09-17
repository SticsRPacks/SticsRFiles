#' @title Transforming a Stics xml file into a text file
#' @description The input file according to his type (ini,plant,tec,station,soil,par)
#' is converted to a text file readable by the Stics model (ficini.txt, ficplt1.txt,...)
#' @param xml_file File path of the input xml file
#' @param java_dir Root directory of the JavaStics installation
#' @param out_dir Directory of the generated file
#' @param out_file Name of the output file
#'
#' @export
#'
stics_xml2txt <- function(xml_file, java_dir, plt_num = 1,out_dir = NULL, out_file = NULL) {

  # defining which xsl file to use according to the input xml file
  #files_types <- c("initialisations","sols","fichierplt","fichiertec")
  xsl_files <- c("ini2txt.xsl","sol2txt.xsl","xml2txt.xsl","xml2txt.xsl",
                 "xml2txt.xsl","xml2txt.xsl","xml2txt.xsl")
  names(xsl_files) <- c("initialisations","sols","fichierplt","fichiertec",
                        "fichiersta","fichierparamgen","fichierpar")
  files_names <- list("ficini.txt","param.sol",list("ficplt1.txt","ficplt2.txt"),
                   list("fictec1.txt","fictec2.txt"),"station.txt","tempoparv6.sti",
                   "tempopar.sti")

  tags = list("_ini","sols.xml","_plt","_tec","_sta","_new","_gen")
  idx = which(unlist(lapply(tags, function(x) grepl(x,xml_file))))
  calc_name = length(idx) > 0

  # Not possible to define output file name
  if ( is.null(out_file) & ! calc_name ) {
    stop("Output file name not found or not provided as argument! ")
  }

  # detecting plt or tec in xml file name
  if ( calc_name & is.null(out_file) ) {
    tag = tags[[idx]]
    if ( tag == "_plt" | tag == "_tec" ) {
      out_file = files_names[[idx]][[plt_num]]
    } else {
      out_file = files_names[[idx]][[1]]
    }
  }

  # getting the input dir if no output dir in args
  if ( is.null(out_dir)) {
    out_dir = dirname(xml_file)
  }

  # defining output path
  out_file_path <- file.path(out_dir,out_file)

  # get the root element name for identifying the file type
  doc <- xml2::read_xml(xml_file)
  filet = xml2::xml_name(doc)
  style_file = file.path(java_dir,"bin/resources/xml/stylesheet",xsl_files[filet])

  # calling the function
  convert_xml2txt(xml_file,style_file, out_file_path)

}
