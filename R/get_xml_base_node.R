#' @title Get an xmlDocument node (set of parameters) from a Stics xml node file template
#' @param file_tag file tag among "usms","sols", "tec"
#' @param form_name formalism name
#' @param stics_version the stics files version to use
#'
#' @export
# TODO: under construction !!!!!!!!!!!!!!!!!
get_xml_base_node <- function(file_tag, form_name=NULL,
                          stics_version = "last") {

  # check/get version
  stics_version <- get_xml_stics_version(stics_version = stics_version)


  files_tags <- c("usms","sols", "tec")
  node_names <- c("usm","sol","intervention")
  parent_nodes <- c("usms","sols","ta")
  formalism_tags <- list()
  formalism_tags$tec <- c("res", "till", "irr","ferN") #, "cutJul", "cutTemp")
  formalism_tags$usms <- c()
  formalism_tags$sols <- c()

  formalism_names <- list()
  formalism_names$tec <- c("supply of organic residus",
                           "soil tillage",
                           "irrigation",
                           "fertilisation") #,
                           #"calendar in days",
                           #"calendar in degree days")

  formalism_names$usms <- c()
  formalism_names$sols <- c()


  if ( ! nargs()) {
    return(list(files_tags = files_tags,
                node_names = node_names,
                parent = parent_nodes,
                form_tags = formalism_tags,
                form_names = formalism_names))
  }



  # see if needed: plt -> cultivars ?

  file_idx <- files_tags %in% file_tag
  if (! any(file_idx) ) {
    stop("unknown file type !")
  }

  node <- node_names[file_idx]

  in_formalism_name <- FALSE
  if ( ! is.null(form_name)) {
    in_formalism_name <- file_tag %in% names(formalism_names)
  }

  if ( in_formalism_name && ! form_name %in% formalism_names[[file_tag]]) {
    stop(paste0("unknown node for ",file_tag))
  }

  form_tag <- formalism_tags[[file_tag]][formalism_names[[file_tag]] %in% form_name]

  if ( ! in_formalism_name ) {
    file_name <- paste0("one_",file_tag,".xml")
  } else {

    file_name <- paste0("one_",form_tag,"_",file_tag,".xml")
  }


  xml_file <- system.file(paste0("extdata/xml/templates/",stics_version,"/",
                                 file_name), package = "SticsRFiles")

  xml_doc <- xmldocument(xml_file)

  base_node_txt <- saveXML(getNodeS(xml_doc,paste0("//",node))[[1]])

  # TODO: see if usefull to call xmlClone or not ?
  new_node <- getNodeSet(xmlParse(base_node_txt),paste0("//",node))[[1]]

  return(new_node)

}
