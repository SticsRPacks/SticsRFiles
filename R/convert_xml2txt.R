#' @title Transforming a Stics xml file into a text file
#' @description The input file according to his type (ini,plant,tec,station,soil,par)
#' is converted to a text file readable by the Stics model (ficini.txt, ficplt1.txt,...)
#' @param xml_file File path of the input xml file
#' @param java_dir Root directory of the JavaStics installation
#' @param plt_num The plant identifier (main crop: 1 ; associated crop: 2)
#' @param out_dir Output directory of the generated file (optional, default:
#' path of the input xml file)
#' @param out_file Name of the output file (optional, default: fixed name for Stics)
#'
#' @examples
#'\dontrun{
#' xml_path <- "/path/to/corn_plt.xml"
#' javastics_path <- "/path/to/javastics/root"
#' convert_xml2txt(xml_file = xml_path, java_dir = javastics_path)
#'}
#'
#' @export
#'
convert_xml2txt <- function(xml_file, java_dir, plt_num = 1,out_dir = NULL, out_file = NULL) {

  # Defining which xsl file to use according to the input xml file
  xsl_files <- c("ini2txt.xsl","sol2txt.xsl","xml2txt.xsl","xml2txt.xsl",
                 "xml2txt.xsl","xml2txt.xsl","xml2txt.xsl")
  names(xsl_files) <- c("initialisations","sols","fichierplt","fichiertec",
                        "fichiersta","fichierparamgen","fichierpar")
  # output text file names
  files_names <- list("ficini.txt","param.sol",list("ficplt1.txt","ficplt2.txt"),
                   list("fictec1.txt","fictec2.txt"),"station.txt","tempoparv6.sti",
                   "tempopar.sti")

  # Using tags from in files names for the xml file type identification
  tags = list("_ini","sols.xml","_plt","_tec","_sta","_new","_gen")
  idx = which(unlist(lapply(tags, function(x) grepl(x,xml_file))))
  calc_name = length(idx) > 0

  # Not possible to define output file name
  if ( base::is.null(out_file) & ! calc_name ) {
    stop("Output file name not found or not provided as argument! ")
  }

  # Detecting plt or tec in xml file name
  if ( calc_name & base::is.null(out_file) ) {
    tag = tags[[idx]]
    if ( tag == "_plt" | tag == "_tec" ) {
      out_file = files_names[[idx]][[plt_num]]
    } else {
      out_file = files_names[[idx]][[1]]
    }
  }

  # Getting the input dir if no output dir in args
  if ( base::is.null(out_dir)) {
    out_dir = dirname(xml_file)
  }

  # Defining output file path
  out_file_path <- file.path(out_dir,out_file)

  # Getting the root element name for identifying the file type
  # TODO: redundancy according to finding tags in files names (see above code, finding idx!)
  doc <- xml2::read_xml(xml_file)
  filet = xml2::xml_name(doc)
  style_file = file.path(java_dir,"bin/resources/xml/stylesheet",xsl_files[filet])

  # calling the function
  convert_xml2txt_int(xml_file,style_file, out_file_path)

}
