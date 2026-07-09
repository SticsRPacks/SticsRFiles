#' Upgrade _tec.xml  file(s) from version 10 to 11
#'
#' @param file xml technical file path or a vector of
#' @param out_dir Output directory path
#@param code_strip Integer indicating if the crop is sown in a strip design: 1 for yes, 2 for no (default)
#@param nrow How many rows in the strip design if `code_strip` is 1 (default 1)
#' @param ... additional argument(s) to pass
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_tec_xml_10_11(
#'   file = file.path(dir_path, "file_tec.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_tec_xml_10_11 <- function(
  file,
  out_dir,
  ...,
  overwrite = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # dot args management ...
  dots_args <- list(...)
  dots_args_names <- names(dots_args)

  # Setting optional args default values
  code_strip <- 2
  nrow <- 1
  if ("code_strip" %in% dots_args_names) {
    code_strip <- dots_args$code_strip
  }
  if ("nrow" %in% dots_args_names) {
    nrow <- dots_args$nrow
  }

  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_tec_xml_10_11(
        file = x,
        out_dir = out_dir,
        code_strip = code_strip,
        nrow = nrow,
        overwrite = overwrite
      )
    })
    return(invisible())
  }

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  format <- get_attrs_values(
    xml_doc,
    path = "//param[@nom='stage_start_irrigauto']",
    attr_list = "format"
  )

  if (format != "character") {
    set_attrs_values(
      xml_doc,
      path = "//param[@nom='stage_start_irrigauto']",
      attr_name = "format",
      values_list = "character"
    )
    remove_attrs(
      xml_doc,
      path = "//param[@nom='stage_start_irrigauto']",
      attr_names = c("min", "max")
    )
  }

  format <- get_attrs_values(
    xml_doc,
    path = "//param[@nom='stage_end_irrigauto']",
    attr_list = "format"
  )

  if (format != "character") {
    set_attrs_values(
      xml_doc,
      path = "//param[@nom='stage_end_irrigauto']",
      attr_name = "format",
      values_list = "character"
    )
    remove_attrs(
      xml_doc,
      path = "//param[@nom='stage_end_irrigauto']",
      attr_names = c("min", "max")
    )
  }

  set_param_value(
    xml_doc,
    param_name = list("stage_start_irrigauto", "stage_end_irrigauto"),
    param_value = list("null", "null")
  )

  # code_strip is either NULL, 1 or 2, and in case it is null, we set it to 2 (no)
  if (is.null(code_strip)) {
    code_strip <- 2
  } else if (code_strip %in% c(1, 2)) {
    code_strip <- as.integer(code_strip)
  } else {
    stop("code_strip must be either 1 (yes) or 2 (no)")
  }

  # nrow must be a positive integer
  nrow <- as.integer(nrow)
  if (nrow < 1) {
    stop("nrow must be a positive integer")
  }

  strip_node <- XML::xmlParseString(
    paste0(
      '<option choix="',
      code_strip,
      '" nom="Is the crop sown in a strip design?" nomParam="code_strip">
        <choix code="1" nom="yes">
          <param format="integer" max="100" min="1" nom="nrow">',
      nrow,
      '</param>
        </choix>
        <choix code="2" nom="no" />
      </option>'
    ),
    addFinalizer = FALSE
  )

  prev_node <- get_nodes(xml_doc, path = "//param[@nom='orientrang']")

  XML::addSibling(prev_node[[1]], XML::xmlClone(strip_node))

  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade _plt.xml file(s) from version 10 to 11
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
# @param stage_const_height Plant height computation parameter (optional)
# @param elongation Plant height computation parameter (optional)
# @param nw_height Plant height computation parameter (optional)
# @param code_shape Plant height computation parameter (optional)
# @param haut_dev_x0 Plant height computation parameter (optional)
# @param haut_dev_k Plant height computation parameter (optional)
#' @param ... additional argument(s) to pass
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#' @param warning Logical for rising warnings, FALSE otherwise
#'
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_plt_xml_10_11(
#'   file = file.path(dir_path, "file_plt.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_plt_xml_10_11 <- function(
  file,
  out_dir,
  # stage_const_height = NULL,
  # elongation = NULL,
  # nw_height = NULL,
  # code_shape = NULL,
  # haut_dev_x0 = NULL,
  # haut_dev_k = NULL,
  ...,
  overwrite = FALSE,
  warning = TRUE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # dot args management ...
  dots_args <- list(...)
  dots_args_names <- names(dots_args)

  # Setting optional args default values
  stage_const_height <- NULL
  elongation <- NULL
  nw_height <- NULL
  code_shape <- NULL
  haut_dev_x0 <- NULL
  haut_dev_k <- NULL
  if ("stage_const_height" %in% dots_args_names) {
    stage_const_height <- dots_args$stage_const_height
  }
  if ("elongation" %in% dots_args_names) {
    elongation <- dots_args$elongation
  }
  if ("nw_height" %in% dots_args_names) {
    nw_height <- dots_args$nw_height
  }
  if ("code_shape" %in% dots_args_names) {
    code_shape <- dots_args$code_shape
  }
  if ("haut_dev_x0" %in% dots_args_names) {
    haut_dev_x0 <- dots_args$haut_dev_x0
  }
  if ("haut_dev_k" %in% dots_args_names) {
    haut_dev_k <- dots_args$haut_dev_k
  }
  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_plt_xml_10_11(
        file = x,
        out_dir = out_dir,
        stage_const_height = stage_const_height,
        elongation = elongation,
        nw_height = nw_height,
        code_shape = code_shape,
        haut_dev_x0 = haut_dev_x0,
        haut_dev_k = haut_dev_k,
        overwrite = overwrite,
        warning = warning
      )
    })
    return(invisible())
  }

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  height_node <- XML::xmlParseString(
    '<formalisme nom="Plant height computation">
    <param format="character" nom="stage_const_height">no</param>
      <param format="real" max="2.0" min="1.0" nom="elongation">1.0</param>
        <param format="real" max="1.0" min="0.0" nom="nw_height">0.0</param>
          <option choix="1" nom="LAI or phasic development relationship" nomParam="code_shape">
            <choix code="1" nom="LAI"/>
              <choix code="2" nom="Development">
                <param format="real" max="5000.0" min="0.0" nom="haut_dev_x0">0.0</param>
                  <param format="real" max="0.05" min="0.0" nom="haut_dev_k">0.0</param>
                    </choix>
                    </option>
                    </formalisme>',
    addFinalizer = TRUE
  )

  leaves_node <- get_nodes(xml_doc, path = "//formalisme[@nom='leaves']")

  XML::addSibling(leaves_node[[1]], XML::xmlClone(height_node))

  # Set values to added parameters if given as func inputs
  # using
  plant <- get_param_value(
    xml_doc = xml_doc,
    param_name = "codeplante"
  )$codeplante
  param <- get_plt_IC_param(crop = plant, warning = warning)

  if (!is.null(stage_const_height)) {
    param$stage_const_height <- stage_const_height
  }
  if (!is.null(elongation)) {
    param$elongation <- elongation
  }
  if (!is.null(nw_height)) {
    param$nw_height <- nw_height
  }
  if (!is.null(code_shape)) {
    param$code_shape <- code_shape
  }
  if (!is.null(haut_dev_x0)) {
    param$haut_dev_x0 <- haut_dev_x0
  }
  if (!is.null(haut_dev_k)) {
    param$haut_dev_k <- haut_dev_k
  }

  lapply(names(param), function(x) {
    if (!is.null(param[[x]])) set_param_value(xml_doc, x, param[[x]])
  })

  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


#' Get crop new parameters for STICS V11
#'
#' @param crop Stics crop code among code list
#' (to get crops codes get_plt_IC_param() )
#' @param message Logical for rising warnings, FALSE otherwise
#'
#' @return A named list of V11 new plant parameters values
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # get plant codes
#' get_plt_IC_param()
#' # get default parameters values
#' get_plt_IC_param(NULL)
#' # get parameters values for poi
#' get_plt_IC_param("poi")
#' }
#'
get_plt_IC_param <- function(crop, warning = TRUE) {
  crops_param <- plt_IC_param_list()
  crop_names <- names(crops_param)

  if (missing(crop)) {
    if (warning) {
      warning(
        "A crop name is mandatory see above list:\n",
        sprintf(fmt = "%s, ", crop_names)
      )
    }
    return(invisible())
  }

  if (is.null(crop)) {
    crop <- "default"
  }

  name <- tolower(crop)

  if (!name %in% crop_names) {
    name <- "default"
    if (warning) {
      val <- unlist(crops_param[[name]])
      val_name <- names(val)
      warning(
        "Unknown crop code name: ",
        crop,
        # crop, "\n See above list: \n",
        # sprintf("%s, ", crop_names),
        # "\n",
        " Using default parameters values!\n",
        paste(sprintf("%s: %s", val_name, val), collapse = ", ")
      )
    }
  }

  return(crops_param[[name]])
}


#' New parameters values for V11 for
#' several crops
#'
#' @return a named list with crop names and parameters values for each crop
#'
#' @keywords internal
#' @noRd
#'
plt_IC_param_list <- function() {
  # pea, wheat, fababean, barley
  param <- list()

  param$default$stage_const_height <- "no"
  param$default$elongation <- 1.0
  param$default$nw_height <- 0.0
  param$default$code_shape <- 1
  param$default$haut_dev_x0 <- 0.0
  param$default$haut_dev_k <- 0.0

  param$poi$stage_const_height <- "mat"
  param$poi$elongation <- 1.0
  param$poi$nw_height <- 0.0
  param$poi$code_shape <- 1
  param$poi$haut_dev_x0 <- 685.395497474724
  param$poi$haut_dev_k <- 0.0113605979397447

  param$ble$stage_const_height <- "no"
  param$ble$elongation <- 1.0
  param$ble$nw_height <- 0.0
  param$ble$code_shape <- 1
  param$ble$haut_dev_x0 <- 886.219548558914
  param$ble$haut_dev_k <- 0.00538369741357949

  param$faba$stage_const_height <- "no"
  param$faba$elongation <- 1.0
  param$faba$nw_height <- 0.45
  param$faba$code_shape <- 1
  param$faba$haut_dev_x0 <- 998.494003649625
  param$faba$haut_dev_k <- 0.00726768251150451

  param$esc$stage_const_height <- "no"
  param$esc$elongation <- 1.0
  param$esc$nw_height <- 0.0
  param$esc$code_shape <- 1
  param$esc$haut_dev_x0 <- 714.811280801783
  param$esc$haut_dev_k <- 0.00891413714283287

  param$tou$stage_const_height <- "no"
  param$tou$elongation <- 1.0
  param$tou$nw_height <- 0.0
  param$tou$code_shape <- 1
  param$tou$haut_dev_x0 <- 645.895914862214
  param$tou$haut_dev_k <- 0.0105505841187171

  param$soj$stage_const_height <- "no"
  param$soj$elongation <- 1.0
  param$soj$nw_height <- 0.0
  param$soj$code_shape <- 1
  param$soj$haut_dev_x0 <- 645.651309418313
  param$soj$haut_dev_k <- 0.00665713018807634

  return(param)
}

#' Upgrade _sta.xml file(s) from STICS version 10 to 11
#'
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_sta_xml_10_11(
#'   file = file.path(dir_path, "file_sta.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_sta_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

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

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  # fix old nom attributes
  codeetp <- XML::xmlParseString(
    '<option choix="1" nom="reading OR calculation of PET" nomParam="codeetp">
            <choix code="1" nom="PET-Penman_reading"/>
            <choix code="2" nom="PET-Penman_calculation"/>
            <choix code="3" nom="PET-Shuttleworth-Wallace_calculation"/>
            <choix code="4" nom="PET-Priestley-Taylor_calculation">
                <param format="real" max="2.0" min="1.0" nom="alphapt">1.26000</param>
            </choix>
        </option>',
    addFinalizer = TRUE
  )

  # get current values
  par_values <- get_param_value(xml_doc, param_name = c("codeetp", "alphapt"))

  # replace codetp node with the previous one
  codetp_node_to_rm <- get_nodes(
    xml_doc,
    path = '//option[@nomParam="codeetp"]'
  )
  XML::removeNodes(codetp_node_to_rm[[1]])

  par_node <- get_nodes(xml_doc, path = '//formalisme[@nom="climate"]')

  XML::addChildren(par_node[[1]], XML::xmlClone(codeetp), at = 0)

  # set values to current values
  set_param_value(
    xml_doc,
    param_name = c("codeetp", "alphapt"),
    param_value = par_values
  )

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


#' Upgrade _ini.xml file(s) from STICS version 10 to 11
#'
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_ini_xml_10_11(
#'   file = file.path(dir_path, "file_ini.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_ini_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

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

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade a param_gen.xml file from STICS version 10 to 11
#'
#' @param file xml param_gen file path or a vector of
#' @param out_dir Output directory path
# @param hauteur_threshold new parameter for V11
# @param par_to_net new parameter for V11
#' @param ... additional argument(s) to pass
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_param_gen_xml_10_11(
#'   file = file.path(dir_path, "param_gen.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_param_gen_xml_10_11 <- function(
  file,
  out_dir,
  # hauteur_threshold = NULL,
  # par_to_net = NULL,
  ...,
  overwrite = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # dot args management ...
  dots_args <- list(...)
  dots_args_names <- names(dots_args)

  # Setting optional args default values
  hauteur_threshold <- NULL
  par_to_net <- NULL

  if ("hauteur_threshold" %in% dots_args_names) {
    hauteur_threshold <- dots_args$hauteur_threshold
  }
  if ("par_to_net" %in% dots_args_names) {
    par_to_net <- dots_args$par_to_net
  }

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  # after <formalisme nom="Simulation options">
  IC_form_node <- XML::xmlParseString(
    '<formalisme nom="Intercropping">
      <param format="real" max="1.0" min="0.05" nom="hauteur_threshold">0.2</param>
      </formalisme>',
    addFinalizer = TRUE
  )
  sim_options_node <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='Simulation options']"
  )
  XML::addSibling(sim_options_node[[1]], XML::xmlClone(IC_form_node))

  # after <param format="real" max="0.6" min="0.4" nom="parsurrg">0.48000</param>
  par2net_node <- XML::xmlParseString(
    '<param format="real" max="1.0" min="0.5" nom="par_to_net">0.83</param>',
    addFinalizer = TRUE
  )

  parsurg_node <- get_nodes(xml_doc, path = "//param[@nom='parsurrg']")
  XML::addSibling(parsurg_node[[1]], XML::xmlClone(par2net_node))

  # Set values to added parameters if given as func inputs
  if (!is.null(hauteur_threshold))
    set_param_value(xml_doc, "hauteur_threshold", hauteur_threshold)
  if (!is.null(par_to_net)) set_param_value(xml_doc, "par_to_net", par_to_net)

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade a param_newform.xml file from STICS version 10 to 11
#'
#' @param file xml param_newform file path or a vector of
#' @param out_dir Output directory path
#' @param ... additional argument(s) to pass
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
# @param use_patho Logical TRUE if code_patho is to be added to the
# param_newform.xml file, FALSE otherwise
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_param_newform_xml_10_11(
#'   file = file.path(dir_path, "param_newform.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_param_newform_xml_10_11 <- function(
  file,
  out_dir,
  ...,
  overwrite = FALSE #,
  #use_patho = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # dot args management ...
  dots_args <- list(...)
  dots_args_names <- names(dots_args)

  # Setting optional args default values
  use_patho <- FALSE

  if ("use_patho" %in% dots_args_names) {
    use_patho <- dots_args$use_patho
  }

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  # renaming humirac parameter to code_humirac
  humirac_node <- get_nodes(
    xml_doc,
    path = "//formalisme/param[@nom='humirac']"
  )

  old_value <- get_param_value(
    xml_doc = xml_doc,
    param_name = "humirac"
  )$humirac

  XML::removeNodes(humirac_node)

  new_node <- XML::xmlParseString(
    '<option choix="2" nom="option to calculate the root growth reduction factor due to soil water content" nomParam="code_humirac">
  <choix code="0" nom="no effect of soil water content"/>
  <choix code="1" nom="using a discontinuous function with a threshold at hminf"/>
  <choix code="2" nom="using a linear function between hminf and hccf"/>
  </option>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='New Roots']"
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node), at = 0)

  set_param_value(
    xml_doc = xml_doc,
    param_name = "code_humirac",
    param_value = old_value
  )

  # adding parameters related to pathogen use, if specified in the
  # function inputs
  if (use_patho) {
    new_node <- XML::xmlParseString(
      '<option choix="2" nom="activation of the mila module for pathogen disease" nomParam="codepatho">
       <choix code="1" nom="yes"/>
       <choix code="2" nom="no"/>
    </option>',
      addFinalizer = TRUE
    )
    parent_node <- get_nodes(
      xml_doc,
      path = "//formalisme[@nom='coupling with pathogen models']"
    )[[1]]

    XML::addChildren(parent_node, XML::xmlClone(new_node), at = 0)
  }

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


#' Upgrade a sols.xml file from STICS version 10 to 11
#'
#' @param file xml sols file path
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_sols_xml_10_11(
#'   file = file.path(dir_path, "sols.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_sols_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade a usms.xml file from STICS version 10 to 11
#'
#' @param file xml usms file path
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @keywords internal
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V10.0")
#'
#' upgrade_usms_xml_10_11(
#'   file = file.path(dir_path, "usms.xml"),
#'   out_dir = tempdir()
#' )
#'
upgrade_usms_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}
