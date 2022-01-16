#' Upgrading _tec.xml file(s) to a newer version
#'
#' @param file Path of a crop management (*_tec.xml) file or a vector of
#' @param param_newform_file Path of the param_newform.xml file corresponding
#' to the file version
#' @param out_dir Output directory path of the generated files
#' @param stics_version Name of the Stics version (VX.Y format)
#' @param target_version Name of the Stics version to upgrade files to (VX.Y format)
#' @param check_version Perform version consistency with in stics_version input
#' with the file version and finally checking if the upgrade is possible
#' allowed to the target_version. If TRUE, param_gen_file is mandatory.
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#' @param ... Additional input arguments
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#' \dontrun{
#' upgrade_tec_xml(file = "/path/to/_tec.xml",
#'                 param_newform_file = "/path/to/param_newform.xml"
#'                 out_dir = "/path/to/directory")
#' }
upgrade_tec_xml <- function(file,
                            param_newform_file,
                            out_dir,
                            stics_version = "9.2",
                            target_version = "10.0",
                            check_version = TRUE,
                            param_gen_file = NULL,
                            overwrite = FALSE,
                            ...) {

  # for verifying output dir existence
  check_dir <- TRUE
  args <- list(...)
  if ("check_dir" %in% names(args)) check_dir <- args$check_dir
  if (check_dir) {
    if (!dir.exists(out_dir)) dir.create(out_dir)
    # for checking only once when multiple files are treated !
    check_dir <- FALSE
  }

  # checking version
  if (check_version) {

    if(is.null(param_gen_file)) stop("param_gen_file must be provided! ")

    min_version <- get_version_num("V9.1")

    # extracting or detecting the Stics version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- check_xml_file_version(file[1],
                                           stics_version,
                                           param_gen_file = param_gen_file)

    if (!file_version) {
      stop("The input version ",stics_version,
           " does not match file version ",
           attr(file_version,"version")," \n",file[1] )
    }

    # Compatibility checks between version and update to target_version
    ver_num <- get_version_num(stics_version)
    if (ver_num < min_version) stop("Files from the version ", stics_version,
                                    " cannot be converted to the version ", target_version)

    # for checking only once when multiple files are treated !
    check_version <- FALSE
  }


  if (length(file) > 1 ) {
    lapply(file, function(x) upgrade_tec_xml(file = x,
                                             param_newform_file = param_newform_file,
                                             out_dir = out_dir,
                                             stics_version = stics_version,
                                             target_version = target_version,
                                             check_version = check_version,
                                             param_gen_file = param_gen_file,
                                             overwrite = overwrite,
                                             check_dir = check_dir)
    )
    return(invisible())
  }

  # Loading the old xml file
  old_doc <- xmldocument(file = file)

  # Setting file stics version
  set_xml_file_version(old_doc, new_version = target_version, overwrite = overwrite)


  # Getting values to keep
  # nodes to be moved ressuite et irecbutoir, no need to keep their values
  # ressuite <- get_param_value(xml_doc = old_doc, param_name = "ressuite")
  # irecbutoir <- get_param_value(xml_doc = old_doc, param_name = "irecbutoir")
  mscoupemini <- get_param_value(xml_doc = old_doc, param_name = "mscoupemini")

  # Keeping the value if any intervention node
  engrais <- get_param_value(xml_doc = old_doc, param_name = "engrais")

  # Keeping the values if any intervention node
  juleclair <- get_param_value(xml_doc = old_doc, param_name = "juleclair")
  nbinfloecl <- get_param_value(xml_doc = old_doc, param_name = "nbinfloecl")
  codeclaircie <- get_param_value(xml_doc = old_doc, param_name = "codeclaircie")


  # Getting nodes to remove or move
  param_names <- c("irecbutoir", "ressuite", "engrais", "mscoupemini", "juleclair", "nbinfloecl" )

  nodes_to_change <- lapply(param_names, function(x)
    getNodeS(docObj = old_doc,
             path = paste0("//param[@nom='",x,"']")))
  # Removing useless nodes
  lapply(nodes_to_change[3:6], function(x) removeNodes(x))

  # Nodes to be moved elsewhere: "irecbutoir", "ressuite"
  nodes_to_move <- nodes_to_change[1:2]


  # tillage option
  new_node <- xmlParseString(
    '<option choix="1" nom="Automatic calculation of the depth of residues incorporation in function of proftrav" nomParam="code_auto_profres">
	<choix code="1" nom="yes">
		<param format="real" max="0.0" min="1.0" nom="resk">0.14</param>
		<param format="real" max="0.0" min="10.0" nom="resz">5.00000</param>
	</choix>
  <choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE)

  parent_node <- getNodeS(docObj = old_doc,
                          path = "//formalisme[@nom='soil tillage']")[[1]]

  addChildren(parent_node, xmlClone(new_node), at=0)

  # codedecisemis param
  new_node <- xmlParseString(
    '<param format="integer" max="20" min="1" nom="nbj_pr_apres_semis">3</param>
<param format="integer" max="20" min="0" nom="eau_mini_decisemis">10</param>
<param format="real" max="1.0" min="0.0" nom="humirac_decisemis">0.75</param>',
    addFinalizer = TRUE)

  new_nodes <- getNodeSet(new_node, path = "//param")

  prev_sibling <- getNodeS(docObj = old_doc,
                           path = "//param[@nom='nbjseuiltempref']")[[1]]
  # to keep the right order
  for (n in seq_along(new_nodes)) {
    new <- xmlClone(new_nodes[[n]])
    addSibling(prev_sibling, new)
    prev_sibling <- new
  }


  # option codedate_irrigauto
  new_node <- xmlParseString(
    '<option choix="3" nom="dates to drive automatic irrigations" nomParam="codedate_irrigauto">
  <choix code="1" nom="dates">
    <param format="integer" max="731" min="0.0" nom="datedeb_irrigauto">0</param>
    <param format="integer" max="731" min="0.0" nom="datefin_irrigauto">0</param>
  </choix>
  <choix code="2" nom="stages">
    <param format="integer" max="731" min="0.0" nom="stage_start_irrigauto">0</param>
    <param format="integer" max="731" min="0.0" nom="stage_end_irrigauto">0</param>
  </choix>
  <choix code="3" nom="no"/>
</option>',
    addFinalizer = TRUE)

  prev_sibling <- getNodeS(docObj = old_doc,
                           path = "//param[@nom='doseirrigmin']")[[1]]

  addSibling(prev_sibling, new_node)


  # intervention + engrais
  # -----------------------
  new_node <- xmlParseString('<colonne nom="engrais"/>',
                             addFinalizer = TRUE)

  parent_node <- getNodeS(docObj = old_doc,
                          path = "//formalisme[@nom='fertilisation']//ta_entete")[[1]]

  addChildren(parent_node, xmlClone(new_node))

  xmlAttrs(parent_node)["nb_colonnes"] <- "3"

  # If any intervention node
  # adding engrais parameter and setting nb_colonnes as in ta_entete
  parent_nodes <- getNodeS(docObj = old_doc,
                           path = "//formalisme[@nom='fertilisation']//ta/intervention")
  if (!is.null(parent_nodes)) {
    lapply(parent_nodes, function(x) addChildren(x, xmlClone(new_node)))
    set_param_value(xml_doc = old_doc, param_name = "engrais", param_value = engrais)
    lapply(parent_nodes, function(x) xmlAttrs(x)["nb_colonnes"] <- "3")
  }

  # param + option: harvest
  new_node <- xmlParseString(
    '<option choix="2" nom="automatic calculation of crop aerial residues in function of user parameterization" nomParam="code_autoressuite">
  <choix code="1" nom="yes">
    <param format="real" max="100.0" min="0.0" nom="Stubblevegratio">0</param>
  </choix>
  <choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE)

  parent_node <- getNodeS(docObj = old_doc,
                          path = "//formalisme[@nom='harvest']")[[1]]

  addChildren(parent_node, xmlClone(new_node), at=0)

  # Moving nodes do not require cloning them (I guess)
  addChildren(parent_node, kids = nodes_to_move, at=0)


  # special techniques: codefauche
  new_node <- list(
    xmlParseString(
      '<option choix="2" nom="dynamic calculation of residual lai on biomass after cutting" nomParam="code_hautfauche_dyn">
  <choix code="1" nom="yes"/>
  <choix code="2" nom="no"/>
</option>',
      addFinalizer = TRUE),
    xmlParseString(
      '<option choix="1" nom="reference thermal time to compute cutting dates " nomParam="codetempfauche">
  <choix code="1" nom="in upvt"/>
  <choix code="2" nom="in udevair"/>
</option>',
      addFinalizer = TRUE)
  )

  parent_node <- getNodeS(docObj = old_doc,
                          path = "//option[@nomParam='codefauche']/choix")[[1]]

  # See if xmlClone is usefull to apply ???
  addChildren(parent_node, kids = new_node, at = 0)


  # special techniques: codemodfauche
  ## Choix "calendar in days"
  new_node <- xmlParseString(
    '<colonne nom="engraiscoupe"/>
<colonne nom="tauxexportfauche"/>
<colonne nom="restit"/>
<colonne nom="mscoupemini"/>',
    addFinalizer = TRUE)

  new_nodes <- getNodeSet(new_node, path = "//colonne")

  parent_node <- getNodeS(docObj = old_doc,
                          path = "//choix[@nom='calendar in days']//ta_entete")[[1]]
  # See if xmlClone is usefull to apply ???
  addChildren(parent_node, kids = new_nodes)

  xmlAttrs(parent_node)["nb_colonnes"] <- "9"

  # If any intervention node
  # add new param nodes: mscoupemini,  tauxexportfauche, restit
  # set kept value of engrais, mscoupemini
  # set default values tauxexportfauche = 1 et restit = 2

  parent_nodes <- getNodeS(docObj = old_doc,
                           path = "//choix[@nom='calendar in days']//ta/intervention")
  if (!is.null(parent_nodes)) {
    # See if xmlClone is usefull to apply ???
    lapply(parent_nodes, function(x) addChildren(x, new_nodes))
    set_param_value(xml_doc = old_doc, param_name = "engraiscoupe", param_value = engrais)
    set_param_value(xml_doc = old_doc, param_name = "mscoupemini", param_value = mscoupemini)
    set_param_value(xml_doc = old_doc, param_name = "tauxexportfauche", param_value = 1)
    set_param_value(xml_doc = old_doc, param_name = "restit", param_value = 2)
    lapply(parent_nodes, function(x) xmlAttrs(x)["nb_colonnes"] <- "9")
  }



  ## Choix "calendar in degree days"
  parent_node <- getNodeS(docObj = old_doc,
                          path = "//choix[@nom='calendar in degree days']//ta_entete")[[1]]
  # See if xmlClone is usefull to apply ???
  addChildren(parent_node, kids = new_nodes)

  xmlAttrs(parent_node)["nb_colonnes"] <- "9"

  # If any intervention node
  # add new param nodes: mscoupemini,  tauxexportfauche, restit
  # set kept value of engrais, mscoupemini
  # set default values tauxexportfauche = 1 et restit = 2
  parent_nodes <- getNodeS(docObj = old_doc,
                           path = "//choix[@nom='calendar in degree days']//ta/intervention")
  if (!is.null(parent_nodes)) {
    # See if xmlClone is usefull to apply ???
    lapply(parent_nodes, function(x) addChildren(x, new_nodes))
    set_param_value(xml_doc = old_doc, param_name = "engraiscoupe", param_value = engrais)
    set_param_value(xml_doc = old_doc, param_name = "mscoupemini", param_value = mscoupemini)
    set_param_value(xml_doc = old_doc, param_name = "tauxexportfauche", param_value = 1)
    set_param_value(xml_doc = old_doc, param_name = "restit", param_value = 2)
    lapply(parent_nodes, function(x) xmlAttrs(x)["nb_colonnes"] <- "9")
  }


  # special techniques: codeclaircie
  new_node <- xmlParseString(
    '<ta nb_interventions="0" nom="thinning management">
   <ta_entete nb_colonnes="2">
     <colonne nom="juleclair"/>
     <colonne nom="nbinfloecl"/>
   </ta_entete>
</ta>',
    addFinalizer = TRUE)

  parent_node <- getNodeS(docObj = old_doc,
                          path = "//option[@nomParam='codeclaircie']//choix[@code='2']")[[1]]

  addChildren(parent_node, xmlClone(new_node))

  # adding intervention if choix codeclaircie == 2 (activation)
  # Using values of juleclair, nbinfloecl got from the old parameters
  if (codeclaircie == 2) {
    # recup noeud ta_entete
    op_node <- xmlClone(getNodeS(old_doc,
                                 paste0("//ta_entete[colonne[@nom='","juleclair","']]"))[[1]])
    xmlName(op_node) <- "intervention"
    parent_node <- getNodeS(docObj = old_doc,
                            path = "//option[@nomParam='codeclaircie']//choix[@code='2']/ta")[[1]]
    addChildren(parent_node, op_node)
    set_param_value(xml_doc = old_doc, param_name = "juleclair", param_value = juleclair)
    set_param_value(xml_doc = old_doc, param_name = "nbinfloecl", param_value = nbinfloecl)

    xmlAttrs(parent_node)["nb_interventions"] <- "1"
  }


  # codejourdes
  new_node <- xmlParseString(
    '<option choix="2" nom="date of plant destruction (for perennial crops only)" nomParam="codejourdes">
  <choix code="1" nom="yes">
    <param format="integer" max="999" min="1" nom="juldes">999</param>
  </choix>
  <choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE)

  parent_node <- getNodeS(docObj = old_doc,
                          path = "//formalisme[@nom='special techniques']")[[1]]

  addChildren(parent_node, xmlClone(new_node))


  # ---------------------------------------------------------------------------------------------------------------------------------------
  # Updating values with param_newform.xml ones
  #
  # codetempfauche
  #
  # <formalisme nom="Moisture test for sowing decision">
  # nbj_pr_apres_semis, eau_mini_decisemis, humirac_decisemis
  #
  # <option choix="3" nom="dates to drive automatic irrigations" nomParam="P_codedate_irrigauto"> -> codedate_irrigauto
  # datedeb_irrigauto datefin_irrigauto stage_start_irrigauto stage_end_irrigauto
  #
  # <option choix="1" nom="Automatic calculation of the depht of residues incorporation in fucntion of proftrav" nomParam="code_auto_profres">
  # code_auto_profres, resk, resz

  old_val <- get_param_xml(param_newform_file,
                           param_name = c("codetempfauche", "nbj_pr_apres_semis", "eau_mini_decisemis", "humirac_decisemis",
                                          "code_auto_profres(1)","resk(1)", "resz(1)", "P_codedate_irrigauto", "datedeb_irrigauto",
                                          "datefin_irrigauto", "stage_start_irrigauto", "stage_end_irrigauto"))[[basename(param_newform_file)]]


  # ERROR ????: using set_param_xml instead after writing new file
  # set_param_value(old_doc,
  #                               param_name = c("codetempfauche", "nbj_pr_apres_semis",
  #                                              "eau_mini_decisemis", "humirac_decisemis",
  #                                              "code_auto_profres","resk", "resz", "codedate_irrigauto",
  #                                              "datedeb_irrigauto",
  #                                              "datefin_irrigauto", "stage_start_irrigauto", "stage_end_irrigauto"),
  #                               param_value = old_val)





  # writing to file _tec.xml
  out_tec <- file.path(out_dir, basename(file))
  write_xml_file(old_doc, out_tec, overwrite)

  # setting new values
  set_param_xml(out_tec,
                param_name = c("codetempfauche", "nbj_pr_apres_semis", "eau_mini_decisemis", "humirac_decisemis",
                               "code_auto_profres","resk", "resz", "codedate_irrigauto", "datedeb_irrigauto",
                               "datefin_irrigauto", "stage_start_irrigauto", "stage_end_irrigauto"),
                old_val,
                overwrite = TRUE)




  free(old_doc@content)
  invisible(gc(verbose = FALSE))

}