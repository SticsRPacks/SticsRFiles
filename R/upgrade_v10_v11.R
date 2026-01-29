#' Upgrade technical file(s) from version 10 to 11
#'
#' @param file xml technical file path or a vector of
#' @param out_dir Output directory path
#' @param code_strip Integer indicating if the crop is sown in a strip design: 1 for yes, 2 for no (default)
#' @param nrow How many rows in the strip design if `code_strip` is 1 (default 1)
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
# @examples
#'
upgrade_tec_xml_10_11 <- function(
  file,
  out_dir,
  code_strip = 2,
  nrow = 1,
  overwrite = FALSE
) {
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

#' Upgrade plant file from version 10 to 11
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param stage_const_height Plant height computation parameter (optional)
#' @param elongation Plant height computation parameter (optional)
#' @param nw_height Plant height computation parameter (optional)
#' @param code_shape Plant height computation parameter (optional)
#' @param haut_dev_x0 Plant height computation parameter (optional)
#' @param haut_dev_k Plant height computation parameter (optional)
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#' @param warning Logical for rising warnings, FALSE otherwise
#'
#'
#' @return None
#'
#' @export
#'
# @examples
#'
upgrade_plt_xml_10_11 <- function(
  file,
  out_dir,
  stage_const_height = NULL,
  elongation = NULL,
  nw_height = NULL,
  code_shape = NULL,
  haut_dev_x0 = NULL,
  haut_dev_k = NULL,
  overwrite = FALSE,
  warning = TRUE
) {
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
          <option choix="1" nom="LAI, Dry mass or phasic development relationship - Principal plant" nomParam="code_shape">
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
#' @param warning Logical for rising warnings, FALSE otherwise
#'
#' @return A named list of V11 new plant parameters values
#'
#' @export
#'
#' @examples
#' # get plant codes
#' get_plt_IC_param()
#' # get default parameters values
#' get_plt_IC_param(NULL)
#' # get parameters values for poi
#' get_plt_IC_param("poi")
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
# @examples
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
  param$poi$code_shape <- 2
  param$poi$haut_dev_x0 <- 685.395497474724
  param$poi$haut_dev_k <- 0.0113605979397447

  param$ble$stage_const_height <- "no"
  param$ble$elongation <- 1.0
  param$ble$nw_height <- 0.0
  param$ble$code_shape <- 2
  param$ble$haut_dev_x0 <- 886.219548558914
  param$ble$haut_dev_k <- 0.00538369741357949

  param$faba$stage_const_height <- "no"
  param$faba$elongation <- 1.0
  param$faba$nw_height <- 0.45
  param$faba$code_shape <- 2
  param$faba$haut_dev_x0 <- 998.494003649625
  param$faba$haut_dev_k <- 0.00726768251150451

  param$esc$stage_const_height <- "no"
  param$esc$elongation <- 1.0
  param$esc$nw_height <- 0.0
  param$esc$code_shape <- 2
  param$esc$haut_dev_x0 <- 714.811280801783
  param$esc$haut_dev_k <- 0.00891413714283287

  param$tou$stage_const_height <- "no"
  param$tou$elongation <- 1.0
  param$tou$nw_height <- 0.0
  param$tou$code_shape <- 2
  param$tou$haut_dev_x0 <- 645.895914862214
  param$tou$haut_dev_k <- 0.0105505841187171

  param$soj$stage_const_height <- "no"
  param$soj$elongation <- 1.0
  param$soj$nw_height <- 0.0
  param$soj$code_shape <- 2
  param$soj$haut_dev_x0 <- 645.651309418313
  param$soj$haut_dev_k <- 0.00665713018807634

  return(param)
}

#' Upgrade xml station file from STICS version 10 to 11
#'
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
# @examples
#'
#'
upgrade_sta_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
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


#' Upgrade xml initialisation file from STICS version 10 to 11
#'
#' @param file xml plant file path or a vector of
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
# @examples
#'
upgrade_ini_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
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

#' Upgrade xml param_gen file from STICS version 10 to 11
#'
#' @param file xml param_gen file path or a vector of
#' @param out_dir Output directory path
#' @param hauteur_threshold new parameter for V11
#' @param par_to_net new parameter for V11
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
upgrade_param_gen_xml_10_11 <- function(
  file,
  out_dir,
  hauteur_threshold = NULL,
  par_to_net = NULL,
  overwrite = FALSE
) {
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

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade xml param_newform file from STICS version 10 to 11
#'
#' @param file xml param_newform file path or a vector of
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
# @examples
#'
upgrade_param_newform_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
  xml_doc <- xmldocument(file)

  # set_version
  check_and_upgrade_xml_version(
    xml_doc,
    from_version = "V10.1.1",
    target_version = "V11.0"
  )

  # renaming humirac parameter to code_humirac
  humirac_node <- SticsRFiles:::get_nodes(
    xml_doc,
    path = "//formalisme/param[@nom='humirac']"
  )

  old_value <- SticsRFiles:::get_param_value(
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

  parent_node <- SticsRFiles:::get_nodes(
    xml_doc,
    path = "//formalisme[@nom='New Roots']"
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node), at = 0)

  SticsRFiles:::set_param_value(
    xml_doc = xml_doc,
    param_name = "code_humirac",
    param_value = old_value
  )

  # write the file
  out_file <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_file, overwrite = overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


#' Upgrade xml sols file from STICS version 10 to 11
#'
#' @param file xml sols file path
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
# @examples
#'
upgrade_sols_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
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

#' Upgrade xml usms file from STICS version 10 to 11
#'
#' @param file xml usms file path
#' @param out_dir Output directory path
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
# @examples
#'
upgrade_usms_xml_10_11 <- function(file, out_dir, overwrite = FALSE) {
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


#' Upgrading a Javastics STICS V10 xml workspace
#'
#' @param workspace JavaStics xml workspace path
#' @param javastics JavaStics (STICS V10) folder path (Optional)
#' @param out_dir   Output directory path
#' @param from_version Starting STICS version (character or numeric)
# @param target_version Target STICS version (character or numeric)
# @param plant
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#' @param verbose   logical, TRUE for displaying a copy message
#' FALSE otherwise (default)
#'
#' @return None
#'
#' @export
#'
# @examples
#'
upgrade_workspace_xml_10_11 <- function(
  workspace,
  javastics = NULL,
  out_dir,
  from_version = "V10.0",
  # target_version = "V11.0",
  overwrite = FALSE,
  verbose = FALSE
) {
  # Just in case, creating the target directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Getting param_gen.xml path
  par_gen <- get_param_gen_file(
    "param_gen.xml",
    workspace,
    javastics
  )

  upgrade_param_gen_xml_10_11(
    file = par_gen,
    out_dir = out_dir,
    overwrite = overwrite
  )

  # Getting param_newform.xml path
  par_new <- get_param_gen_file(
    "param_newform.xml",
    workspace,
    javastics
  )

  upgrade_param_newform_xml_10_11(
    file = par_new,
    out_dir = out_dir,
    overwrite = overwrite
  )

  # Converting usms.xml file
  usms <- file.path(workspace, "usms.xml")
  upgrade_usms_xml_10_11(usms, out_dir, overwrite = overwrite)

  # Converting sols.xml file
  sols <- file.path(workspace, "sols.xml")
  upgrade_sols_xml_10_11(sols, out_dir, overwrite = overwrite)

  # Converting station files (*_sta.xml)
  sta_files <- get_in_files(in_dir_or_files = workspace, kind = "sta")
  upgrade_sta_xml_10_11(sta_files, out_dir, overwrite = overwrite)

  # Converting initialisation files (*_ini.xml)
  ini_files <- get_in_files(in_dir_or_files = workspace, kind = "ini")
  upgrade_ini_xml_10_11(ini_files, out_dir, overwrite = overwrite)

  # Converting crop management files (*_tec.xml)
  tec_files <- get_in_files(in_dir_or_files = workspace, kind = "tec")
  upgrade_tec_xml_10_11(
    tec_files,
    out_dir,
    code_strip = 2,
    nrow = 1,
    overwrite = overwrite
  )

  # Upgrading plant files
  # if a plant sub directory exists in workspace
  usms_plt_files <-
    unique(unlist(get_param_xml(file = usms, param = "fplt")$usms$fplt))
  usms_plt_files <- usms_plt_files[usms_plt_files != "null"]
  plant_files <- get_in_files(
    in_dir_or_files = file.path(workspace, "plant"),
    kind = "plt"
  )
  plant_idx <- usms_plt_files %in% basename(plant_files)
  full_plant_files <- file.path(workspace, "plant", usms_plt_files[plant_idx])

  usms_plt_files <- usms_plt_files[!plant_idx]

  if (!is.null(javastics)) {
    plant_files <- get_in_files(
      in_dir_or_files = file.path(javastics, "plant"),
      kind = "plt"
    )

    plant_idx <- usms_plt_files %in% basename(plant_files)

    # Combining javastics and workspace plant files
    full_plant_files <- c(
      full_plant_files,
      file.path(javastics, "plant", usms_plt_files[plant_idx])
    )
  }

  if (length(full_plant_files) > 0) {
    # For creating a sub-directory in workspace for upgraded plant files
    plant_out_dir <- file.path(out_dir, "plant")
    if (!dir.exists(plant_out_dir)) {
      dir.create(plant_out_dir)
    }

    upgrade_plt_xml_10_11(
      file = full_plant_files,
      out_dir = plant_out_dir,
      overwrite = overwrite
    )
  }

  # Other files types copy from the source workspace,
  # or from javastics "example"
  # dir for *.mod files if they do not exist in the workspace
  workspace_files_copy(
    workspace = workspace,
    javastics = javastics,
    out_dir = out_dir,
    overwrite = overwrite,
    verbose = verbose
  )
}


#' Check if the versions for upgrading xml files are compatible
#'
#' @param xml_doc An SticsRFiles xml_doc object
#' @param from_version STICS starting version
#' @param target_version STICS target version
#'
#' @description
#' Defining if the starting version to use for files upgrading process
#' is compatible with the target version.
#' Versions may be given either as character strings (i.e. "V9.2")
#' or numerical value (i.e. 9.2)
#'
#' @keywords internal
#' @noRd
#'
check_and_upgrade_xml_version <- function(
  xml_doc,
  from_version,
  target_version
) {
  from_version_num <- get_version_num(from_version)
  target_version_num <- get_version_num(target_version)

  if ((target_version_num - from_version_num) < 1e-06) {
    stop(
      "The target version ",
      target_version,
      " must be higher than the initial version ",
      from_version
    )
  }
  # Checking if target version is supported
  # raising an error if not!
  check_version_compat(target_version)

  # checking actual version consistency between from_version
  # and file version
  from_version_major <- get_major_version(from_version)
  file_version <- get_xml_file_version(xml_doc)
  if (
    !is.null(file_version) &&
      !get_major_version(file_version) %in% from_version_major
  ) {
    stop(
      "file has a wrong starting version !",
      "must be a",
      paste0(from_version_major, ".x")
    )
  }
  # Setting new file STICS version
  set_xml_file_version(xml_doc, new_version = target_version)
}

#' Get the major version number of a STICS version
#'
#' @param version Character string representing the STICS version (i.e. "V9.2")
#' @param to_num Logical, TRUE for converting character version to numeric one,
#' FALSE otherwise (default)
#'
#' @keywords internal
#' @noRd
#'
get_major_version <- function(version, to_num = FALSE) {
  str_version <- strsplit(x = version, split = "\\.")[[1]][1]

  if (!to_num) {
    return(str_version)
  }

  get_version_num(str_version)
}
