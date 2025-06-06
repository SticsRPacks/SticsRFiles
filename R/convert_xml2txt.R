#' @title Transforming a STICS xml file into a text file
#' @description The input file according to his type
#' (ini,plant,tec,station,soil,par)
#' is converted to a text file readable by the STICS model
#' (ficini.txt, ficplt1.txt,...)
#' @param file Path (including name) of the xml file to convert
#' @param plant_id The plant identifier (main crop: 1 ; associated crop: 2)
#' @param soil_name Soil name (optional, required for soil file)
#' @param out_dir Path of the directory where to generate the file.
#' Optional, set to the path of the input xml file by default
#' @param save_as Name of the output file
#' (optional, default: fixed name for STICS)
#' @param stics_version the STICS files version to use (optional,
#' default to latest).
#'
#' @return None
#'
#' @examples
#' xml_path <- file.path(get_examples_path("xml"), "file_plt.xml")
#' convert_xml2txt(file = xml_path)
#' xml_path <- file.path(get_examples_path("xml"), "sols.xml")
#' convert_xml2txt(file = xml_path, soil_name = "soil_rice")
#'
#' @export
#'
convert_xml2txt <- function(
    file,
    plant_id = 1,
    soil_name = NULL,
    out_dir = NULL,
    save_as = NULL,
    stics_version = "latest") {
  # Defining which xsl file to use according to the input xml file
  xsl_files <- c(
    "ini2txt.xsl",
    "sol2txt.xsl",
    "xml2txt.xsl",
    "xml2txt.xsl",
    "xml2txt.xsl",
    "xml2txt.xsl",
    "xml2txt.xsl"
  )
  # Defining which xsl file to use according to the input xml file
  names(xsl_files) <- c(
    "initialisations",
    "sols",
    "fichierplt",
    "fichiertec",
    "fichiersta",
    "fichierparamgen",
    "fichierpar"
  )
  # output text file names
  files_names <- list(
    "ficini.txt",
    "param.sol",
    list("ficplt1.txt", "ficplt2.txt"),
    list("fictec1.txt", "fictec2.txt"),
    "station.txt",
    "tempoparv6.sti",
    "tempopar.sti"
  )
  # Using tags from in files names for the xml file type identification
  tags <- list(
    "_ini\\.xml",
    "sols\\.xml",
    "_plt\\.xml",
    "_tec\\.xml",
    "_sta\\.xml",
    "_newform\\.xml",
    "_gen\\.xml"
  )
  idx <- which(unlist(lapply(tags, function(x) grepl(x, file))))
  calc_name <- length(idx) > 0

  # Not possible to define output file name
  if (base::is.null(save_as) && !calc_name) {
    stop("Output file name not found or not provided as argument! ")
  }

  # Detecting plt or tec in xml file name
  if (calc_name && base::is.null(save_as)) {
    tag <- tags[[idx]]
    if (tag == "_plt\\.xml" || tag == "_tec\\.xml") {
      save_as <- files_names[[idx]][[plant_id]]
    } else {
      save_as <- files_names[[idx]][[1]]
    }
  }

  # Getting the input dir if no output dir in args
  if (base::is.null(out_dir)) {
    out_dir <- dirname(file)
  }

  # Defining output file path
  out_file_path <- file.path(out_dir, save_as)

  # Getting the root element name for identifying the file type
  # TODO: redundancy according to finding tags in files names
  # (see above code, finding idx!)
  doc <- xml2::read_xml(file)
  filet <- xml2::xml_name(doc)

  # Calling get_examples_path
  xsl_dir <- get_examples_path("xsl", stics_version = stics_version)

  style_file <- file.path(xsl_dir, xsl_files[filet])

  # specific case for soil file
  if (filet == "sols") {
    if (is.null(soil_name)) {
      stop("Soil name not provided as argument! ")
    }

    # check if the soil_name is in the sols.xml file
    soils_names <- get_soils_list(file.path(dirname(file), "sols.xml"))

    if (!soil_name %in% soils_names) {
      stop(paste(
        "Soil name ",
        soil_name,
        " not found in sols.xml file !"
      ))
    }

    # generate sol2txt.xsl in the tempdir() directory
    ret <- gen_sol_xsl_file(soil_name, stics_version)

    # getting the path of the generated xsl file
    style_file <- attr(ret, "path")

    # if any writing problem
    if (!ret) {
      stop(
        "Problem when generating soil xsl file !\n",
        style_file
      )
    }
  }

  # calling the xml conversion function
  status <- convert_xml2txt_int(file, style_file, out_file_path)

  return(status)
}
