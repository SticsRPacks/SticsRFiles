# fix tec et appel depuis
#' @export
upgrade_tec_xml_10_11 <- function(file,
                                  out_dir,
                                  overwrite = FALSE) {

  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_tec_xml_10_11(
        file = x,
        out_dir = out_dir,
        overwrite = overwrite
      )
    })
    return(invisible())
  }


  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")

  format <- SticsRFiles:::get_attrs_values(xml_doc, path = "//param[@nom='stage_start_irrigauto']",
                                           attr_list = "format")

  if (format != "character") {

    SticsRFiles:::set_attrs_values(xml_doc,
                                   path = "//param[@nom='stage_start_irrigauto']",
                                   attr_name = "format",
                                   values_list = "character" )
    SticsRFiles:::remove_attrs(xml_doc, path = "//param[@nom='stage_start_irrigauto']", attr_names = c("min", "max"))
  }

  format <- SticsRFiles:::get_attrs_values(xml_doc, path = "//param[@nom='stage_end_irrigauto']",
                                           attr_list = "format")

  if (format != "character") {

    SticsRFiles:::set_attrs_values(xml_doc,
                                   path = "//param[@nom='stage_end_irrigauto']",
                                   attr_name = "format",
                                   values_list = "character" )
    SticsRFiles:::remove_attrs(xml_doc, path = "//param[@nom='stage_end_irrigauto']", attr_names = c("min", "max"))
  }

  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))

}


#' @export
upgrade_plt_xml_10_11 <- function(file,
                                  out_dir,
                                  crop = NULL,
                                  stage_const_height = NULL,
                                  elongation = NULL,
                                  nw_height = NULL,
                                  code_shape = NULL,
                                  haut_dev_x0= NULL,
                                  haut_dev_k = NULL,
                                  nrow = NULL,
                                  overwrite = FALSE) {



  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_plt_xml_10_11(
        file = x,
        out_dir = out_dir,
        crop,
        stage_const_height = stage_const_height,
        elongation = elongation,
        nw_height = nw_height,
        code_shape = code_shape,
        haut_dev_x0= haut_dev_x0,
        haut_dev_k = haut_dev_k,
        nrow = nrow,
        overwrite = overwrite
      )
    })
    return(invisible())
  }

  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")



  height_node <- XML::xmlParseString('<formalisme nom="Plant height computation">
    <param format="character" nom="stage_const_height">no</param>
      <param format="real" max="2.0" min="1.0" nom="elongation">1.0</param>
        <param format="real" max="1.0" min="0.0" nom="nw_height">0.0</param>
          <option choix="2" nom="LAI, Dry mass or phasic development relationship - Principal plant" nomParam="code_shape">
            <choix code="1" nom="LAI"/>
              <choix code="2" nom="Development">
                <param format="real" max="5000.0" min="0.0" nom="haut_dev_x0">0.0</param>
                  <param format="real" max="0.05" min="0.0" nom="haut_dev_k">0.0</param>
                    </choix>
                    </option>
                    </formalisme>',
                                     addFinalizer = TRUE)

  nrow_node <- XML::xmlParseString('<param format="real" max="10.0" min="1.0" nom="nrow">1</param>',
                                   addFinalizer = TRUE)
  leaves_node <- SticsRFiles:::get_nodes(xml_doc, path = "//formalisme[@nom='leaves']")

  XML::addSibling(leaves_node[[1]], XML::xmlClone(height_node))

  dfolhaut_node <- SticsRFiles:::get_nodes(xml_doc, path = "//param[@nom='dfolhaut']")

  XML::addSibling(dfolhaut_node[[1]], XML::xmlClone(nrow_node))

  # Set values to added parameters if given as func inputs
  # using
  param <- get_plt_IC_param(crop = crop)

  if(!is.null(stage_const_height)) param$stage_const_height <- stage_const_height
  if(!is.null(elongation)) param$elongation <- elongation
  if(!is.null(nw_height)) param$nw_height <- nw_height
  if(!is.null(code_shape)) param$code_shape <- code_shape
  if(!is.null(haut_dev_x0)) param$haut_dev_x0 <- haut_dev_x0
  if(!is.null(haut_dev_k)) param$haut_dev_k <- haut_dev_k
  if(!is.null(nrow)) param$nrow <- nrow

  lapply(names(param), function(x) {
    if (!is.null(param[[x]])) set_param_value(xml_doc, x,param[[x]])
  })


  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))

}


get_plt_IC_param <- function(crop) {

  crops_param <- plt_IC_param_list()
  crop_names <- names(crops_param)

  if (missing(crop)) {
    warning("A crop name is mandatory see above list:\n",
            sprintf(fmt = "%s, ",crop_names))
    return()
  }

  if (is.null(crop)) crop <- "default"

  name <- tolower(crop)

  if (!name %in% crop_names) {
    warning("Unknown crop code name: ",
            crop, "\n See above list: \n",
            sprintf("%s, ", crop_names),
            "\n",
            "Using default parameters values!")
    name <- "default"
  }

  return(crops_param[[name]])

}

plt_IC_param_list <- function() {

  # pea, wheat, fababean, barley
  param <- list()

  param$default$stage_const_height <- "no"
  param$default$elongation <- 1.0
  param$default$nw_height <- 0.0
  param$default$code_shape <- 2
  param$default$haut_dev_x0 <- 0.0
  param$default$haut_dev_k <- 0.0
  param$default$nrow <- 1

  param$pea$stage_const_height <- "mat"
  param$pea$elongation <- 1.0
  param$pea$nw_height <- 0.0
  param$pea$code_shape <- 2
  param$pea$haut_dev_x0 <- 685.395497474724
  param$pea$haut_dev_k <- 0.0113605979397447
  param$pea$nrow <- 1

  param$wheat$stage_const_height <- "no"
  param$wheat$elongation <- 1.0
  param$wheat$nw_height <- 0.0
  param$wheat$code_shape <- 2
  param$wheat$haut_dev_x0 <- 886.219548558914
  param$wheat$haut_dev_k <- 0.00538369741357949
  param$wheat$nrow <- 1

  param$fababean$stage_const_height <- "no"
  param$fababean$elongation <- 1.0
  param$fababean$nw_height <- 0.45
  param$fababean$code_shape <- 2
  param$fababean$haut_dev_x0 <- 998.494003649625
  param$fababean$haut_dev_k <- 0.00726768251150451
  param$fababean$nrow <- 1

  param$barley$stage_const_height <- "no"
  param$barley$elongation <- 1.0
  param$barley$nw_height <- 0.0
  param$barley$code_shape <- 2
  param$barley$haut_dev_x0 <- 714.811280801783
  param$barley$haut_dev_k <- 0.00891413714283287
  param$barley$nrow <- 1

  return(param)

}


#' @export
upgrade_sta_xml_10_11 <- function(file,
                                  out_dir,
                                  overwrite = FALSE) {

  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_sta_xml_10_11(
        file = x,
        out_dir = out_dir,
        overwrite = overwrite
      )
    })
    return(invisible())
  }

  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")

  # fix old nom attributes
  codeetp <- '<option choix="1" nom="reading OR calculation of PET" nomParam="codeetp">
            <choix code="1" nom="PET-Penman_reading"/>
            <choix code="2" nom="PET-Penman_calculation"/>
            <choix code="3" nom="PET-Shuttleworth-Wallace_calculation"/>
            <choix code="4" nom="PET-Priestley-Taylor_calculation">
                <param format="real" max="2.0" min="1.0" nom="alphapt">1.26000</param>
            </choix>
        </option>'

  # get current values
  # replace codetp node with the previous one
  # set values to current values

  # write the file
  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))

}


upgrade_ini_xml_10_11 <- function(file,
                                  out_dir,
                                  overwrite = FALSE) {

  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_ini_xml_10_11(
        file = x,
        out_dir = out_dir,
        overwrite = overwrite
      )
    })
    return(invisible())
  }

  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")


  # voir position de magrain0
  # verifier position si après lai0
  # sinon get_node et ajout après lai0

  # write the file
  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' @export
upgrade_param_gen_xml_10_11 <- function(file,
                                        out_dir,
                                        hauteur_threshold = NULL,
                                        par_to_net = NULL,
                                        overwrite = FALSE) {

  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")


  # after <formalisme nom="Simulation options">
  IC_form <- '<formalisme nom="Intercropping">
		<param format="real" max="1.0" min="0.05" nom="hauteur_threshold">0.2</param>
    </formalisme>'

  # after <param format="real" max="0.6" min="0.4" nom="parsurrg">0.48000</param>
  par2net <- '<param format="real" max="1.0" min="0.5" nom="par_to_net">0.83</param>'

  # Set values to added parameters if given as func inputs

  # write the file
  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))

}

#' @export
upgrade_param_newform_xml_10_11 <- function(file,
                                       out_dir,
                                       overwrite = FALSE) {

  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")

  # write the file
  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' @export
upgrade_sols_xml_10_11 <- function(file,
                                  out_dir,
                                  overwrite = FALSE) {

  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")

  # write the file
  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))

}

#' @export
upgrade_usms_xml_10_11 <- function(file,
                                   out_dir,
                                   overwrite = FALSE) {

  xml_doc <- SticsRFiles:::xmldocument(file)


  # set_version
  check_and_upgrade_xml_version(xml_doc, from_version = "V10.1.1", target_version = "V11.0")

  # write the file
  out_file <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))

}


#' @export
upgrade_workspace_xml_10_11 <- function(workspace,
                                        javastics,
                                        out_dir,
                                        from_version = "V10.0",
                                        target_version = "V11.0",
                                        plant = FALSE,
                                        overwrite = FALSE,
                                        ...) {

  # Just in case, creating the target directory
  if (!dir.exists(out_dir)) dir.create(out_dir)

  # Getting param_gen.xml path
  par_gen <- SticsRFiles:::get_param_gen_file(type = "param_gen.xml",
                                              workspace,
                                              javastics)

  upgrade_param_gen_xml_10_11(file = par_gen,
                              out_dir = out_dir,
                              overwrite = overwrite)


  # Getting param_newform.xml path
  par_new <- SticsRFiles:::get_param_gen_file(
    type = "param_newform.xml",
    workspace, javastics
  )

  upgrade_param_newform_xml_10_11(file = par_new,
                             out_dir = out_dir,
                             overwrite = overwrite)

  # Converting usms.xml file
  usms <- file.path(workspace, "usms.xml")
  upgrade_usms_xml_10_11(usms, out_dir, overwrite = overwrite)


  # Converting sols.xml file
  sols <- file.path(workspace, "sols.xml")
  upgrade_sols_xml_10_11(sols, out_dir, overwrite = overwrite)

  # Converting station files (*_sta.xml)
  sta_files <- SticsRFiles:::get_in_files(in_dir_or_files = workspace, kind = "sta")
  upgrade_sta_xml_10_11(sta_files, out_dir, overwrite = overwrite)

  # Converting initialisation files (*_ini.xml)
  ini_files <- SticsRFiles:::get_in_files(in_dir_or_files = workspace, kind = "ini")
  upgrade_ini_xml_10_11(ini_files, out_dir, overwrite = overwrite)

  # Converting crop management files (*_tec.xml)
  tec_files <- SticsRFiles:::get_in_files(in_dir_or_files = workspace, kind = "tec")
  upgrade_tec_xml_10_11(tec_files, out_dir, overwrite = overwrite)

  # Upgrading plant files
  # if a plant sub directory exists in workspace

  usms_plt_files <-
    unique(unlist(get_param_xml(file = usms, param = "fplt")$usms$fplt))
  usms_plt_files <- usms_plt_files[usms_plt_files != "null"]
  plant_files <- SticsRFiles:::get_in_files(
    in_dir_or_files = file.path(workspace, "plant"),
    kind = "plt"
  )
  plant_idx <- usms_plt_files %in% basename(plant_files)
  full_plant_files <- file.path(workspace, "plant", usms_plt_files[plant_idx])

  usms_plt_files <- usms_plt_files[!plant_idx]

  plant_files <- SticsRFiles:::get_in_files(
    in_dir_or_files = file.path(javastics, "plant"),
    kind = "plt"
  )

  plant_idx <- usms_plt_files %in% basename(plant_files)

  # combining javastics and workspace plant files
  full_plant_files <- c(full_plant_files,
                        file.path(javastics,
                                  "plant",
                                  usms_plt_files[plant_idx]))


  if (length(full_plant_files) > 0) {
    # For creating a sub-directory in workspace for upgraded plant files
    plant_out_dir <- file.path(out_dir, "plant")
    if (!dir.exists(plant_out_dir)) dir.create(plant_out_dir)

    upgrade_plt_xml_10_11(file = full_plant_files,
                          out_dir = plant_out_dir,
                          overwrite = overwrite)
  }
}


#' @export
check_and_upgrade_xml_version <- function(xml_doc,
                                          from_version,
                                          target_version) {



  from_version_num <- SticsRFiles:::get_version_num(from_version)
  target_version_num <- SticsRFiles:::get_version_num(target_version)

  if ((target_version_num - from_version_num) < 1e-06)
    stop("The target version ",
         target_version,
         " must be higher than the initial version ",
         from_version
    )

  # Checking if target version is supported
  # raising an error if not!
  SticsRFiles:::check_version_compat(target_version)

  # if (!SticsRFiles:::is.xml_document(file_or_doc)) {
  #   file_or_doc <- SticsRFiles:::xmldocument(file_or_doc)
  # }

  # checking actual version consistency between from_version
  # and file version
  from_version <- get_major_version(from_version)
  file_version <- get_major_version(SticsRFiles:::get_xml_file_version(xml_doc))

  #print(xml_doc)
  #print(from_version)
  #print(file_version)
  if (!file_version %in% from_version)
    stop("file has a wrong starting version !",
         "must be a",
         paste0(from_version,".x"))

  # Setting new file STICS version
  SticsRFiles:::set_xml_file_version(xml_doc,
                                     new_version = target_version
  )
}

#' @export
get_major_version <- function(version, to_num = FALSE) {
  str_version <- strsplit(x = version, split = "\\.")[[1]][1]

  if(!to_num)
    return(str_version)

  SticsRFiles:::get_version_num(str_version)
}
