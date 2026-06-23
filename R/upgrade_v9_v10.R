#' Upgrade _tec.xml file(s) from STICS version 9 to 10
#'
#' @param file Path of a crop management (*_tec.xml) file or a vector of
#' @param out_dir Output directory path of the generated files
#' @param param_newform_file Path of the param_newform.xml file corresponding
#' to the file version
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
# @param ... Additional input arguments
#'
#' @return None
#'
#' @export
#'
#' @details See get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_tec_xml_9_10(
#'   file = file.path(dir_path, "file_tec.xml"),
#'   out_dir = tempdir(),
#'   param_newform_file = file.path(dir_path, "param_newform.xml"),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_tec_xml_9_10 <- function(
  file,
  out_dir,
  param_newform_file,
  param_gen_file,
  overwrite = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_tec_xml_9_10(
        file = x,
        param_newform_file = param_newform_file,
        out_dir = out_dir,
        param_gen_file = param_gen_file,
      )
    })
    return(invisible())
  }

  # Loading the old xml file
  xml_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10"
  )

  # Getting values to keep
  mscoupemini <- get_param_value(xml_doc = xml_doc, param_name = "mscoupemini")

  # Keeping the value if any intervention node
  engrais <- get_param_value(xml_doc = xml_doc, param_name = "engrais")

  # Keeping the values if any intervention node
  juleclair <- get_param_value(xml_doc = xml_doc, param_name = "juleclair")
  nbinfloecl <- get_param_value(xml_doc = xml_doc, param_name = "nbinfloecl")
  codeclaircie <- get_param_value(
    xml_doc = xml_doc,
    param_name = "codeclaircie"
  )

  # Getting nodes to remove or move
  param_names <- c(
    "irecbutoir",
    "ressuite",
    "engrais",
    "mscoupemini",
    "juleclair",
    "nbinfloecl"
  )

  nodes_to_change <- lapply(param_names, function(x) {
    get_nodes(
      xml_doc,
      path = paste0("//param[@nom='", x, "']")
    )
  })
  # Removing useless nodes
  lapply(nodes_to_change[3:6], function(x) XML::removeNodes(x))

  # Nodes to be moved elsewhere: "irecbutoir", "ressuite"
  nodes_to_move <- nodes_to_change[1:2]

  # tillage option
  new_node <- XML::xmlParseString(
    '<option choix="1" nom="Automatic calculation of the depth of residues
    incorporation in function of proftrav" nomParam="code_auto_profres">
	<choix code="1" nom="yes">
		<param format="real" max="0.0" min="1.0" nom="resk">0.14</param>
		<param format="real" max="0.0" min="10.0" nom="resz">5.00000</param>
	</choix>
  <choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='soil tillage']"
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node), at = 0)

  # codedecisemis param
  new_node <- XML::xmlParseString(
    '<param format="integer" max="20" min="1" nom="nbj_pr_apres_semis">3</param>
<param format="integer" max="20" min="0" nom="eau_mini_decisemis">10</param>
<param format="real" max="1.0" min="0.0" nom="humirac_decisemis">0.75</param>',
    addFinalizer = TRUE
  )

  new_nodes <- XML::getNodeSet(new_node, path = "//param")

  prev_sibling <- get_nodes(
    xml_doc,
    path = "//param[@nom='nbjseuiltempref']"
  )[[1]]
  # to keep the right order
  for (n in seq_along(new_nodes)) {
    new <- XML::xmlClone(new_nodes[[n]])
    XML::addSibling(prev_sibling, new)
    prev_sibling <- new
  }

  # option codedate_irrigauto
  new_node <- XML::xmlParseString(
    '<option choix="3" nom="dates to drive automatic irrigations"
    nomParam="codedate_irrigauto">
  <choix code="1" nom="dates">
    <param format="integer" max="731" min="0.0"
    nom="datedeb_irrigauto">0</param>
    <param format="integer" max="731" min="0.0"
    nom="datefin_irrigauto">0</param>
  </choix>
  <choix code="2" nom="stages">
    <param format="integer" max="731" min="0.0"
    nom="stage_start_irrigauto">0</param>
    <param format="integer" max="731" min="0.0"
    nom="stage_end_irrigauto">0</param>
  </choix>
  <choix code="3" nom="no"/>
</option>',
    addFinalizer = TRUE
  )

  prev_sibling <- get_nodes(
    xml_doc,
    path = "//param[@nom='doseirrigmin']"
  )[[1]]

  XML::addSibling(prev_sibling, new_node)

  # intervention , engrais
  # -----------------------
  new_node <- XML::xmlParseString(
    '<colonne nom="engrais"/>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='fertilisation']//ta_entete"
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node))

  XML::xmlAttrs(parent_node)["nb_colonnes"] <- "3"

  # If any intervention node
  # adding engrais parameter and setting nb_colonnes as in ta_entete
  parent_nodes <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='fertilisation']//ta/intervention"
  )
  if (!is.null(parent_nodes)) {
    lapply(
      parent_nodes,
      function(x) {
        XML::addChildren(x, XML::xmlClone(new_node))
      }
    )
    set_param_value(
      xml_doc = xml_doc,
      param_name = "engrais",
      param_value = engrais
    )
    lapply(parent_nodes, function(x) XML::xmlAttrs(x)["nb_colonnes"] <- "3")
  }

  # param, option: harvest
  new_node <- XML::xmlParseString(
    '<option choix="2" nom="automatic calculation of crop aerial residues
    in function of user parameterization" nomParam="code_autoressuite">
  <choix code="1" nom="yes">
    <param format="real" max="100.0" min="0.0" nom="Stubblevegratio">0</param>
  </choix>
  <choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='harvest']"
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node), at = 0)

  # Moving nodes do not require cloning them (I guess)
  XML::addChildren(parent_node, kids = nodes_to_move, at = 0)

  # special techniques: codefauche
  new_node <- list(
    XML::xmlParseString(
      '<option choix="2" nom="dynamic calculation of residual lai
      on biomass after cutting" nomParam="code_hautfauche_dyn">
  <choix code="1" nom="yes"/>
  <choix code="2" nom="no"/>
</option>',
      addFinalizer = TRUE
    ),
    XML::xmlParseString(
      '<option choix="1" nom="reference thermal time to compute
      cutting dates " nomParam="codetempfauche">
  <choix code="1" nom="in upvt"/>
  <choix code="2" nom="in udevair"/>
</option>',
      addFinalizer = TRUE
    )
  )

  parent_node <- get_nodes(
    xml_doc,
    path = "//option[@nomParam='codefauche']/choix"
  )[[1]]

  # See if xmlClone is useful to apply ???
  XML::addChildren(parent_node, kids = new_node, at = 0)

  # special techniques: codemodfauche
  ## Choix "calendar in days"
  new_node <- XML::xmlParseString(
    '<colonne nom="engraiscoupe"/>
<colonne nom="tauxexportfauche"/>
<colonne nom="restit"/>
<colonne nom="mscoupemini"/>',
    addFinalizer = TRUE
  )

  new_nodes <- XML::getNodeSet(new_node, path = "//colonne")

  parent_node <- get_nodes(
    xml_doc,
    path = "//choix[@nom='calendar in days']//ta_entete"
  )[[1]]
  # See if xmlClone is useful to apply ???
  XML::addChildren(parent_node, kids = new_nodes)

  XML::xmlAttrs(parent_node)["nb_colonnes"] <- "9"

  # If any intervention node
  # add new param nodes: mscoupemini,  tauxexportfauche, restit
  # set kept value of engrais, mscoupemini
  # set default values tauxexportfauche = 1 et restit = 2

  parent_nodes <- get_nodes(
    xml_doc,
    path = "//choix[@nom='calendar in days']//ta/intervention"
  )
  if (!is.null(parent_nodes)) {
    # See if xmlClone is useful to apply ???
    lapply(parent_nodes, function(x) XML::addChildren(x, new_nodes))
    set_param_value(
      xml_doc = xml_doc,
      param_name = "engraiscoupe",
      param_value = engrais
    )
    set_param_value(
      xml_doc = xml_doc,
      param_name = "mscoupemini",
      param_value = mscoupemini
    )
    set_param_value(
      xml_doc = xml_doc,
      param_name = "tauxexportfauche",
      param_value = 1
    )
    set_param_value(
      xml_doc = xml_doc,
      param_name = "restit",
      param_value = 2
    )
    lapply(parent_nodes, function(x) XML::xmlAttrs(x)["nb_colonnes"] <- "9")
  }

  ## Choix "calendar in degree days"
  parent_node <- get_nodes(
    xml_doc,
    path = "//choix[@nom='calendar in degree days']//ta_entete"
  )[[1]]
  # See if xmlClone is useful to apply ???
  XML::addChildren(parent_node, kids = new_nodes)

  XML::xmlAttrs(parent_node)["nb_colonnes"] <- "9"

  # If any intervention node
  # add new param nodes: mscoupemini,  tauxexportfauche, restit
  # set kept value of engrais, mscoupemini
  # set default values tauxexportfauche = 1 et restit = 2
  parent_nodes <- get_nodes(
    xml_doc,
    path = "//choix[@nom='calendar in degree days']//ta/intervention"
  )
  if (!is.null(parent_nodes)) {
    # See if xmlClone is useful to apply ???
    lapply(parent_nodes, function(x) XML::addChildren(x, new_nodes))
    set_param_value(
      xml_doc = xml_doc,
      param_name = "engraiscoupe",
      param_value = engrais
    )
    set_param_value(
      xml_doc = xml_doc,
      param_name = "mscoupemini",
      param_value = mscoupemini
    )
    set_param_value(
      xml_doc = xml_doc,
      param_name = "tauxexportfauche",
      param_value = 1
    )
    set_param_value(xml_doc = xml_doc, param_name = "restit", param_value = 2)
    lapply(parent_nodes, function(x) XML::xmlAttrs(x)["nb_colonnes"] <- "9")
  }

  # special techniques: codeclaircie
  new_node <- XML::xmlParseString(
    '<ta nb_interventions="0" nom="thinning management">
   <ta_entete nb_colonnes="2">
     <colonne nom="juleclair"/>
     <colonne nom="nbinfloecl"/>
   </ta_entete>
</ta>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    path = "//option[@nomParam='codeclaircie']//choix[@code='2']"
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node))

  # adding intervention if choix codeclaircie == 2 (activation)
  # Using values of juleclair, nbinfloecl got from the old parameters
  if (codeclaircie == 2) {
    # recup noeud ta_entete
    op_node <- XML::xmlClone(get_nodes(
      xml_doc,
      paste0("//ta_entete[colonne[@nom='", "juleclair", "']]")
    )[[1]])
    XML::xmlName(op_node) <- "intervention"
    parent_node <- get_nodes(
      xml_doc,
      path = "//option[@nomParam='codeclaircie']//choix[@code='2']/ta"
    )[[1]]
    XML::addChildren(parent_node, op_node)
    set_param_value(
      xml_doc = xml_doc,
      param_name = "juleclair",
      param_value = juleclair
    )
    set_param_value(
      xml_doc = xml_doc,
      param_name = "nbinfloecl",
      param_value = nbinfloecl
    )

    XML::xmlAttrs(parent_node)["nb_interventions"] <- "1"
  }

  # codejourdes
  new_node <- XML::xmlParseString(
    '<option choix="2" nom="date of plant destruction
    (for perennial crops only)" nomParam="codejourdes">
  <choix code="1" nom="yes">
    <param format="integer" max="999" min="1" nom="juldes">999</param>
  </choix>
  <choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='special techniques']"
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node))

  # ----------------------------------------------------------------------------
  # Updating values with param_newform.xml ones
  #

  param_names <- c(
    "codetempfauche",
    "nbj_pr_apres_semis",
    "eau_mini_decisemis",
    "humirac_decisemis",
    "code_auto_profres(1)",
    "resk(1)",
    "resz(1)",
    "P_codedate_irrigauto",
    "datedeb_irrigauto",
    "datefin_irrigauto",
    "stage_start_irrigauto",
    "stage_end_irrigauto"
  )
  old_val <- get_param_xml(param_newform_file, param = param_names)[[basename(
    param_newform_file
  )]]

  # writing to file _tec.xml
  out_tec <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_tec, overwrite)

  # setting new values
  param_names <- c(
    "codetempfauche",
    "nbj_pr_apres_semis",
    "eau_mini_decisemis",
    "humirac_decisemis",
    "code_auto_profres",
    "resk",
    "resz",
    "codedate_irrigauto",
    "datedeb_irrigauto",
    "datefin_irrigauto",
    "stage_start_irrigauto",
    "stage_end_irrigauto"
  )
  set_param_xml(
    file = out_tec,
    param = param_names,
    values = old_val,
    overwrite = TRUE
  )

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


#' Upgrade _plt.xml file(s) from STICS version 9 to 10
#'
#' @param file Path of an plant (*_plt.xml) file or a vector of
#' @param out_dir Output directory path of the generated files
#' @param param_newform_file Path of the param_newform.xml file corresponding
#' to the file version
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
# @param ... Additional input arguments
#'
#' @return None
#'
#' @export
#'
#' @details See get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_plt_xml_9_10(
#'   file = file.path(dir_path, "file_plt.xml"),
#'   out_dir = tempdir(),
#'   param_newform_file = file.path(dir_path, "param_newform.xml"),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_plt_xml_9_10 <- function(
  file,
  out_dir,
  param_newform_file,
  param_gen_file,
  overwrite = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_plt_xml_9_10(
        file = x,
        out_dir = out_dir,
        param_newform_file = param_newform_file,
        param_gen_file = param_gen_file,
        overwrite = overwrite
      )
    })
    return(invisible())
  }

  # local initialization, before loadinf RData
  jvc_data <- NULL

  # Loading the old xml file
  xml_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10"
  )

  # Parameters to move to varietal parameters ----------------------------------
  #
  # Coming from other formalisms than varietal ones
  # Storing parameters values to set them to varietal ones
  param_names_to_varietal <- c(
    "phobase",
    "phosat",
    "stdordebour",
    "bdens",
    "hautbase",
    "hautmax",
    "dlaimax",
    "dlaimaxbrut",
    "innsen",
    "rapsenturg",
    "extin",
    "ktrou",
    "temin",
    "teopt",
    "slamax",
    "tigefeuil",
    "nbjgrain",
    "nbgrmin",
    "vitircarb",
    "vitircarbT",
    "stdrpnou",
    "nbinflo",
    "inflomax",
    "pentinflores",
    "vitpropsucre",
    "vitprophuile",
    "vitirazo",
    "tgellev10",
    "tgeljuv10",
    "tgelveg10",
    "psisto",
    "psiturg",
    "deshydbase"
  )
  param_values_to_varietal <- get_param_value(xml_doc, param_names_to_varietal)

  # nodes existing outside of 'cultivar parameters' formalism
  nodes_to_rm <- lapply(param_names_to_varietal, function(x) {
    get_nodes(
      xml_doc,
      path = paste0(
        "//formalisme[@nom!='cultivar parameters']//param[@nom='",
        x,
        "']"
      )
    )
  })

  # Checking nodes : all must be not NULL
  nodes_null <- unlist(lapply(nodes_to_rm, is.null))
  if (any(nodes_null)) {
    stop("missing nodes, not a v9.1 or 9.2 _plt.xml file")
  }

  # Removing nodes
  lapply(nodes_to_rm, function(x) XML::removeNodes(x))

  # Already exist in varietal parameters
  # Storing parameters values before applying new varietal parameters structure
  param_names_keep <- c(
    "stlevamf",
    "stamflax",
    "stlevdrp",
    "stflodrp",
    "stdrpdes",
    "pgrainmaxi",
    "adens",
    "croirac",
    "durvieF",
    "jvc",
    "sensiphot",
    "stlaxsen",
    "stsenlan",
    "nbgrmax",
    "stdrpmat",
    "afruitpot",
    "dureefruit"
  )

  param_values_keep <- get_param_value(xml_doc, param_names_keep)

  # Checking values: list names ==  param_names_keep
  if (!all(param_names_keep %in% names(param_values_keep))) {
    stop("Missing values, , not a v9.1 or 9.2 _plt.xml file")
  }

  # General plant parameters ---------------------------------------------------
  #
  # adding codephot_part
  new_node <- XML::xmlParseString(
    '<option choix="2" nom="effect of decreasing photoperiod on
    biomass allocation" nomParam="codephot_part">
    <choix code="1" nom="yes"/>
    <choix code="2" nom="no"/>
    </option>',
    addFinalizer = TRUE
  )

  # test if the node exists
  node_exists <- !is.null(get_nodes(
    xml_doc,
    '//option[@nomParam="codephot_part"]'
  ))

  if (node_exists) {
    stop(
      "codephot_part already exists, not a v9.1 or 9.2 _plt.xml file"
    )
  }

  parent_node <- get_nodes(
    xml_doc,
    '//option[@nomParam="codephot"]/choix[@code="1"]'
  )[[1]]

  XML::addChildren(parent_node, XML::xmlClone(new_node))

  # moving nbfeuilplant
  node_to_move <- get_nodes(xml_doc, path = "//param[@nom='nbfeuilplant']")[[1]]
  prev_sibling <- get_nodes(xml_doc, path = "//param[@nom='laiplantule']")[[1]]

  XML::addSibling(prev_sibling, node_to_move)

  # Recalculating irazomax
  irazomax_calc <- calc_irazomax(
    get_param_value(xml_doc, "irmax")$irmax,
    param_values_to_varietal$vitircarb,
    param_values_to_varietal$vitirazo
  )

  new_node <- XML::xmlParseString(
    paste0(
      '<param format="real" max="1.0" min="0.01" nom="irazomax">',
      irazomax_calc,
      "</param>"
    ),
    addFinalizer = TRUE
  )

  # adding irazomax node
  parent_node <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='yield formation']"
  )[[1]]
  XML::addChildren(parent_node, new_node, at = 0)

  message(
    xml_doc@name,
    ": be aware that irazomax is a new parameter and its value (",
    irazomax_calc,
    ")\nis estimated using some other parameters values.\n",
    paste0(
      "But this value needs to be ajusted according to ",
      "species and varieties "
    ),
    "\n"
  )

  # moving irmax
  node_to_move <- get_nodes(xml_doc, path = "//param[@nom='irmax']")[[1]]
  parent_node <- get_nodes(
    xml_doc,
    path = "//option[@nomParam='codeir']/choix[@code='1']"
  )[[1]]

  XML::addChildren(parent_node, node_to_move)

  # add codedisrac option node
  new_node <- XML::xmlParseString(
    '<option choix="2" nom="Standard root distribution" nomParam="codedisrac">
    <choix code="1" nom="yes">
    <param format="real" max="0.01" min="0.00001" nom="kdisrac">0.00158</param>
    <param format="real" max="1.0" min="0.0" nom="alloperirac">0.23</param>
    </choix>
    <choix code="2" nom="no"/>
    </option>',
    addFinalizer = TRUE
  )
  # before sibling
  # <option choix="2" nom="N effect on root distribution" nomParam="codazorac">
  prev_sibling <- get_nodes(xml_doc, '//option[@nomParam="codazorac"]')[[1]]
  XML::addSibling(prev_sibling, new_node, after = FALSE)

  # Changing varietal section structure ----------------------------------------
  #
  # Remove nodes from old doc for varietal content (under variete nodes)
  nodes_to_rm <- get_nodes(xml_doc, path = "//variete/*")
  lapply(nodes_to_rm, function(x) XML::removeNodes(x))

  # Add under each variete node new nodes
  var_nodes <- XML::xmlParseString(
    '<formalismev nom="phenological stages">
			<param format="real" max="6000.0" min="0.0" nom="stlevamf">275</param>
			<param format="real" max="6000.0" min="0.0" nom="stamflax">375</param>
			<param format="real" max="6000.0" min="0.0" nom="stlevdrp">837</param>
			<param format="real" max="500.0" min="0.0" nom="stflodrp">0</param>
			<param format="real" max="900.0" min="0.0" nom="stdrpdes">700</param>
			<param format="integer" max="70" min="0" nom="jvc">55</param>
			<optionv nom="codebfroid">
				<param code="3" format="real" max="20000.0" min="0.0"
				nom="stdordebour">0</param>
			</optionv>
			<optionv nom="codephot">
				<param code="1" format="real" max="1.0" min="0.0"
				nom="sensiphot">0</param>
				<param code="1" format="real" max="24.0" min="0.0"
				nom="phobase">6.3</param>
				<param code="1" format="real" max="24.0" min="0.0"
				nom="phosat">20</param>
			</optionv>
</formalismev>
<formalismev nom="leaves">
			<param format="real" max="0.0" min="-2.0" nom="adens">-0.6</param>
			<param format="real" max="200.0" min="1.0" nom="bdens">7.00000</param>
			<param format="real" max="2.0" min="0.0" nom="hautbase">0.00000</param>
			<param format="real" max="5.0" min="0.1" nom="hautmax">1.2</param>
			<param format="real" max="2.0" min="0.2" nom="khaut">0.70</param>
			<param format="real" max="500.0" min="10.0" nom="durvieF">200</param>
			<optionv nom="codlainet">
				<param code="1" format="real" max="6000.0" min="0.0"
				nom="stlaxsen">675.</param>
				<param code="1" format="real" max="6000.0" min="0.0"
				nom="stsenlan">110.</param>
				<param code="1" format="real" max="0.5" min="5.0E-6"
				nom="dlaimax">0.00047</param>
				<param code="2" format="real" max="0.5" min="5.0E-6"
				nom="dlaimaxbrut">0.00047</param>
				<param code="2" format="real" max="1.0" min="-2.0"
				nom="innsen">0.17</param>
				<param code="2" format="real" max="1.5" min="0.5"
				nom="rapsenturg">0.50</param>
			</optionv>
</formalismev>
<formalismev nom="radiation interception">
			<optionv nom="codetransrad">
				<param code="1" format="real" max="1.5" min="0.1"
				nom="extin">0.50</param>
				<param code="2" format="real" max="2.0" min="0.1"
				nom="ktrou">1.00</param>
			</optionv>
</formalismev>
<formalismev nom="shoot biomass growth">
			<param format="real" max="15.0" min="-10.0" nom="temin">0.0</param>
			<param format="real" max="30.0" min="10.0" nom="teopt">12.0</param>
</formalismev>
<formalismev nom="biomass partitioning">
			<param format="real" max="500.0" min="50.0" nom="slamax">350</param>
			<param format="real" max="3.0" min="0.0" nom="tigefeuil">0.50</param>
</formalismev>
<formalismev nom="yield formation">
			<param format="real" max="5.0" min="0.0"
			nom="pgrainmaxi">0.03970</param>
			<param format="real" max="0.01" min="0.0010"
			nom="vitpropsucre">0.00000</param>
			<param format="real" max="0.01" min="0.0010"
			nom="vitprophuile">0.00000</param>
			<param format="real" max="0.04" min="0.0010"
			nom="vitirazo">0.01450</param>
		    <param format="real" max="0.02" min="-0.02"
		    nom="deshydbase">0.008000</param>
			<optionv nom="codeindetermin">
				<param code="1" format="integer" max="40" min="5"
				nom="nbjgrain">30</param>
				<param code="1" format="real" max="10000.0" min="0.0"
				nom="nbgrmin">6000</param>
				<param code="1" format="real" max="1000000.0" min="0.0"
				nom="nbgrmax">30000</param>
				<param code="1" format="real" max="2000.0" min="0.0"
				nom="stdrpmat">700</param>
				<optionv code="1" nom="codeir">
					<param code="1" format="real" max="0.02" min="0.001"
					nom="vitircarb">0.0107</param>
					<param code="2" format="real" max="0.002" min="5.0E-5"
					nom="vitircarbT">0</param>
				</optionv>
				<param code="2" format="real" max="20.0" min="0.5"
				nom="afruitpot">.00000</param>
				<param code="2" format="real" max="2000.0" min="10.0"
				nom="dureefruit">.00000</param>
				<param code="2" format="real" max="6000.0" min="0.0"
				nom="stdrpnou">-999</param>
				<optionv code="2" nom="codcalinflo">
					<param code="1" format="real" max="100.0" min="0.0"
					nom="nbinflo">-999</param>
				    <param code="2" format="real" max="100.0" min="0.0"
				    nom="inflomax">-999</param>
					<param code="2" format="real" max="10.0" min="0.0"
					nom="pentinflores">-999</param>
				</optionv>
			</optionv>
</formalismev>
<formalismev nom="roots">
			<param format="real" max="0.5" min="0.0" nom="croirac">0.12</param>
</formalismev>
<formalismev nom="frost">
			<optionv nom="codgellev">
				<param code="2" format="real" max="0.0" min="-25.0"
				nom="tgellev10">-4.0</param>
			</optionv>
			<optionv nom="codgeljuv">
				<param code="2" format="real" max="0.0" min="-25.0"
				nom="tgeljuv10">-10.0</param>
			</optionv>
			<optionv nom="codgelveg">
				<param code="2" format="real" max="0.0" min="-25.0"
				nom="tgelveg10">-4.5</param>
			</optionv>
</formalismev>
<formalismev nom="water stress">
			<param format="real" max="25.0" min="1.0" nom="psisto">15</param>
			<param format="real" max="15.0" min="1.0" nom="psiturg">4</param>
			<param format="real" max="0.3" min="0.05" nom="swfacmin">0.1</param>
</formalismev>',
    addFinalizer = TRUE
  )

  # adding var nodes under each "variete" node
  var_parent_nodes <- get_nodes(xml_doc, path = "//variete")
  lapply(
    var_parent_nodes,
    function(x) {
      XML::addChildren(x, kids = XML::xmlChildren(XML::xmlClone(var_nodes)))
    }
  )

  #
  # Set values of all kept parameter values (removed nodes or moved)
  # to new nodes
  # param_values_to_varietal
  set_param_value(xml_doc, param_names_to_varietal, param_values_to_varietal)
  set_param_value(xml_doc, param_names_keep, param_values_keep)

  # Special case: jvc not any more under an option choice
  # Checking values for jvc, if == -999, and replacing with
  # a value set in new files provided with JavaSTICS
  # Values stores in a jvc.RData in the package.
  #
  #
  # If the file name is the same as a provided plt file.
  # Get values
  # - get variete from current file
  # - intersect names with those in loaded RData file
  # - set values for parameters == -999 for common varieties
  # to those loaded

  load(
    file.path(
      get_examples_path(
        file_type = "xml_param",
        stics_version = "V10"
      ),
      "jvc_data.RData"
    )
  )

  # get varieties
  current_var <- get_param_value(xml_doc, "variete")

  if (basename(file) %in% names(jvc_data)) {
    common_var <-
      jvc_data[[basename(file)]][["variete"]] %in% current_var$variete

    if (any(common_var)) {
      values <- get_param_value(
        xml_doc,
        param_name = "jvc",
        parent_name = "variete",
        parent_sel_attr = current_var[common_var]
      )$jvc

      values_999_ids <- which(values == -999)
      if (length(values_999_ids) > 0) {
        jvc_values <- jvc_data[[basename(file)]]$jvc[common_var][values_999_ids]
        set_param_value(
          xml_doc,
          param_name = "jvc",
          param_value = jvc_values,
          ids = values_999_ids
        )
      }
    }
  }

  # In case of the file name not found in the list for the V10 version
  # Check if still any -999 values
  values <- get_param_value(xml_doc, "jvc")$jvc
  values_999_ids <- which(values == -999)
  if (length(values_999_ids) > 0) {
    message(
      xml_doc@name,
      paste0(
        ": be aware that jvc is from now a mandatory parameter",
        "and its value must be fixed !\n"
      ),
      "for all varieties: \n",
      current_var[values_999_ids],
      "\n"
    )
  }

  # Adding new nodes
  #
  # codemortalracine
  # after <option choix="1" nom="driving temperature" nomParam="codetemprac">
  new_node <- XML::xmlParseString(
    '<option choix="2" nom="calculation of the root death at cutting date
    for grasslands" nomParam="codemortalracine">
	<choix code="1" nom="function of dry matter production between
	two successives cut">
		<param format="real" max="1.0" min="0.0" nom="coefracoupe">0.5</param>
	</choix>
    <choix code="2" nom="no specific root death at cutting"/>
</option>',
    addFinalizer = TRUE
  )

  prev_sibling <- get_nodes(xml_doc, "//*[@nomParam='codetemprac']")[[1]]
  XML::addSibling(prev_sibling, XML::xmlClone(new_node))

  # code_WangEngel
  # in <choix code="1" nom="daily temperatures">
  new_node <- XML::xmlParseString(
    '<option choix="2" nom=" Wang et Engel (1998)" nomParam="code_WangEngel">
	<choix code="1" nom="Wang et Engel">
		<param format="real" max="30" min="0" nom="tdoptdeb">11.7</param>
	</choix>
	<choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    "//*[@nomParam='codegdhdeb']/choix[@code='1']"
  )[[1]]
  XML::addChildren(parent_node, new_node)

  # Adding 2 option nodes
  # in <formalisme nom="partitioning of biomass in organs">
  #
  new_nodes <- XML::xmlParseString(
    '<option choix="2" nom="Simulation of Nitrogen and Carbon reserves"
    nomParam="code_acti_reserve">
      <choix code="1" nom="yes">
      <param format="real" max="1.0" min="0.0" nom="propresP">0.47</param>
      <param format="real" max="1.0" min="0.0" nom="PropresPN">0.6</param>
      <param format="real" max="1.0" min="0.0" nom="Efremobil">0.05</param>
      <param format="real" max="1.0" min="0.0" nom="Propres">0.10</param>
      <param format="real" max="1.0" min="0.0" nom="tauxmortresP">0.0025</param>
      <param format="real" max="50.0" min="0.0" nom="Parazoper">25</param>
      <param format="real" max="100.0" min="0.0" nom="ParazoTmorte">30</param>
      <param format="real" max="0.1" min="0" nom="inilai">0.001</param>
      </choix>
      <choix code="2" nom="no">
      <param format="real" max="100.0" min="0.0" nom="resplmax">0.66</param>
      </choix>
      </option>
    <option choix="2" nom="Calculation of the stem elongation stage for
    perenial grasslands" nomParam="codemontaison">
		<choix code="1" nom="yes"/>
		<choix code="2" nom="no"/>
</option>
<option choix="2" nom="Simulation of tiller dynamics for grasslands"
nomParam="codedyntalle">
	<choix code="1" nom="yes">
		<option choix="1" nom="Choice of the ratio used to calculate
		tiller mortality" nomParam="codetranspitalle">
			<choix code="1" nom="et/etm"/>
			<choix code="2" nom="ep/eop"/>
		</option>
			<param format="real" max="0.00001" min="0.0000025"
			nom="SurfApex">0.000005</param>
			<param format="real" max="0.04" min="0.01"
			nom="SeuilMorTalle">0.02</param>
			<param format="real" max="0.2" min="0.1"
			nom="SigmaDisTalle">0.1</param>
			<param format="real" max="0.01" min="0.00001"
			nom="VitReconsPeupl">0.001</param>
			<param format="real" max="1200" min="400"
			nom="SeuilReconsPeupl">800</param>
			<param format="real" max="5000" min="3000"
			nom="MaxTalle">4000.0</param>
			<param format="real" max="2.0" min="0.5"
			nom="SeuilLAIapex">1.0</param>
			<param format="real" max="10.0" min="1.0"
			nom="tigefeuilcoupe">5.0</param>
	</choix>
	<choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(
    xml_doc,
    "//formalisme[@nom='partitioning of biomass in organs']"
  )[[1]]
  XML::addChildren(parent_node, kids = unlist(XML::xmlChildren(new_nodes)))

  # <formalisme nom="roots">
  # rayon
  #
  new_node <- XML::xmlParseString(
    '<param format="real" max="0.07" min="0.005" nom="rayon">0.02</param>'
  )
  prev_sibling <- get_nodes(xml_doc, "//*[@nom='contrdamax']")[[1]]
  XML::addSibling(prev_sibling, new_node)

  # adding 2 option nodes
  new_nodes <- XML::xmlParseString(
    '<option choix="2" nom="Preferential allocation of biomass to roots
    in case of water or N stress" nomParam="code_stress_root">
    <choix code="1" nom="yes"/>
    <choix code="2" nom="no"/>
    </option>
    <option choix="2" nom="rootdeposition activation"
    nomParam="code_rootdeposition">
    <choix code="1" nom="yes">
    <param format="real" max="60.0" min="10.0" nom="parazorac">-999</param>
    <option choix="2" nom="simulation of 2 roots classes"
    nomParam="code_diff_root">
    <choix code="1" nom="yes">
    <param format="real" max="4.0" min="0.0" nom="lvmax">-999</param>
    <param format="real" max="10.0" min="1.0" nom="rapdia">-999</param>
    <param format="real" max="0.80" min="0.05" nom="RTD">-999</param>
    <param format="real" max="1.0" min="0.0" nom="propracfmax">-999</param>
    </choix>
    <choix code="2" nom="no"/>
    </option>
    </choix>
    <choix code="2" nom="no"/>
    </option>',
    addFinalizer = TRUE
  )

  parent_node <- get_nodes(xml_doc, "//choix[@nom='true density']")[[1]]

  XML::addChildren(parent_node, kids = unlist(XML::xmlChildren(new_nodes)))

  # -----------------------------------------------------------
  # Update param values
  #
  # from param_gen.xml
  # khaut, rayon
  param_gen_values <- get_param_xml(
    file = param_gen_file,
    param = c("rayon", "khaut")
  )[[1]]
  set_param_value(
    xml_doc,
    param_name = c("rayon", "khaut"),
    param_value = param_gen_values
  )

  # from param_newform.xml
  # coefracoupe(1), coefracoupe(2) -> coefracoupe
  param_newform_values <- get_param_xml(
    file = param_newform_file,
    param = c("coefracoupe(1)", "coefracoupe(2)")
  )[[1]]

  if (length(unique(unlist(param_newform_values))) > 1) {
    stop(
      "Multiple values of coefracoupe in param_gen.xml file"
    )
  }

  set_param_value(
    xml_doc,
    param_name = "coefracoupe",
    param_value = param_newform_values[[1]]
  )

  # Updating other values than nodes values (i.e. nodes attributes values)
  #
  # Changing param min / max wrong attributes values
  # hautbase => 0.1
  # <param format="real" max="2.0" min="0.0" nom="hautbase">0</param>
  nodes_to_change <- get_nodes(xml_doc, path = "//param[@nom='hautbase']")
  if (!is.null(nodes_to_change)) {
    set_attrs_values(
      xml_doc,
      path = "//param[@nom='hautbase']",
      attr_name = "min",
      values_list = "0.1"
    )
  }
  #
  # Changing options' "choix",  "nom" attribute values
  #
  # oui to yes, non to no
  nodes_to_change <- get_nodes(xml_doc, path = "//choix[@nom='oui']")
  if (!is.null(nodes_to_change)) {
    set_attrs_values(
      xml_doc,
      path = "//choix[@nom='oui']",
      attr_name = "nom",
      values_list = "yes"
    )
  }
  nodes_to_change <- get_nodes(xml_doc, path = "//choix[@nom='non']")
  if (!is.null(nodes_to_change)) {
    set_attrs_values(
      xml_doc,
      path = "//choix[@nom='non']",
      attr_name = "nom",
      values_list = "no"
    )
  }

  # Writing to file _plt.xml
  out_plt <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_plt, overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


calc_irazomax <- function(irmax, vitircarb, vitirazo) {
  irazomax <- (irmax / vitircarb) * vitirazo

  irazomax <- pmin(1., irazomax)

  if (is.nan(irazomax) || irazomax > 1) {
    irazomax <- 1
  }

  return(round(irazomax, digits = 3))
}

#' Upgrade _sta.xml file(s) from STICS version 9 to 10
#'
#' @param file Path of a station (*_sta.xml) file or a vector of
#' @param out_dir Output directory path of the generated files
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
# @param ... Additional input arguments
#'
#' @return None
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_sta_xml_9_10(
#'   file = file.path(dir_path, "file_sta.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_sta_xml_9_10 <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_sta_xml_9_10(
        file = x,
        param_gen_file = param_gen_file,
        out_dir = out_dir,
        # stics_version = stics_version,
        # target_version = target_version,
        # check_version = check_version,
        overwrite = overwrite # ,
        # check_dir = check_dir
      )
    })
    return(invisible())
  }

  # Loading xml file
  xml_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10.0"
  )

  # Getting old concrr value
  concrr <- get_param_xml(param_gen_file, "concrr")$param_gen.xml$concrr

  # Getting new parameter
  concrr_node <- XML::xmlParseString(
    '<param format="real" max="3.0" min="0.0" nom="concrr">0.02000</param>',
    addFinalizer = TRUE
  )

  # Getting the preceeding sibling node
  prev_sibling <- get_nodes(xml_doc, "//*[@nom='NH3ref']")[[1]]
  XML::addSibling(node = prev_sibling, XML::xmlClone(concrr_node), after = TRUE)

  # Setting concrr value
  set_param_value(xml_doc, param_name = "concrr", param_value = concrr)

  # Adding snow formalism new node
  new_node <- XML::xmlParseString(
    '<formalisme nom="Climate with snow">
    <option choix="3" nom="Select snow model" nomParam="codemodlsnow">
    <choix code="1" nom="1-Unused"/>
    <choix code="2" nom="2-Unused"/>
    <choix code="3" nom="3-My only choice">
    <param format="real" max="-0.5" min="-3" nom="tsmax">-2</param>
    <param format="real" max="1.5" min="0.5" nom="trmax">1</param>
    <param format="real" max="2" min="1" nom="DKmax">1.50000</param>
    <param format="real" max="2.5" min="1.5" nom="Kmin">2</param>
    <param format="real" max="1" min="0" nom="Tmf">0.5</param>
    <param format="real" max="0.01" min="0" nom="SWrf">0.01</param>
    <param format="real" max="200" min="10" nom="Pns">100</param>
    <param format="real" max="0.05" min="0" nom="E">0.02</param>
    <param format="real" max="15" min="5" nom="prof">10</param>
    <param format="real" max="0" min="-1" nom="tminseuil">-0.5</param>
    <param format="real" max="0.5" min="-0.5" nom="tmaxseuil">0</param>
    </choix>
    </option>
    </formalisme>',
    addFinalizer = TRUE
  )

  par_node <- get_nodes(xml_doc, path = "/fichiersta")[[1]]
  XML::addChildren(par_node, XML::xmlClone(new_node))

  # Writing to file _sta.xml
  write_xml_file(
    xml_doc,
    file.path(out_dir, basename(file)),
    overwrite = overwrite
  )

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


#' Upgrade _ini.xml file(s) from STICS version 9 to 10
#'
#' @param file Path of an initialization (*_ini.xml) file or a vector of
#' @param out_dir Output directory path of the generated files
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_ini_xml_9_10(
#'   file = file.path(dir_path, "file_ini.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_ini_xml_9_10 <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_ini_xml_9_10(
        file = x,
        out_dir = out_dir,
        param_gen_file = param_gen_file,
        overwrite = overwrite
      )
    })
    return(invisible())
  }

  # Loading the old xml file
  xml_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10.0"
  )

  # Keeping old values
  rm_names <- c("masec0", "QNplante0", "resperenne0")
  old_values <- get_param_value(xml_doc, rm_names)

  # Removing useless nodes
  rm_nodes <- unlist(lapply(rm_names, function(x) {
    unlist(get_nodes(
      xml_doc,
      path = paste0("//", x)
    ))
  }))
  lapply(rm_nodes, function(x) XML::removeNodes(x))

  # Adding new option node
  # including old nodes masec0,QNplante0,restemp0
  # (previously named resperennes0)

  str_1 <- paste0(
    '<option choix="2" nom="Simulation of Nitrogen and Carbon',
    ' reserves" nomParam="code_acti_reserve">\n'
  )
  str_2 <-
    '<choix code="1" nom="yes">
		<maperenne0>0</maperenne0>
		<QNperenne0>0</QNperenne0>
		<masecnp0>0</masecnp0>
		<QNplantenp0>0</QNplantenp0>
	</choix>
	<choix code="2" nom="no">
		<masec0>0</masec0>
		<QNplante0>0</QNplante0>
		<restemp0>0</restemp0>
	</choix>
 </option>'

  str <- paste0(str_1, str_2)

  new_node <- XML::xmlParseString(str, addFinalizer = TRUE)

  # Getting zrac0 node
  prev_sibling <- unlist(get_nodes(xml_doc, "//zrac0"))

  # Adding new node
  lapply(prev_sibling, function(x) XML::addSibling(x, XML::xmlClone(new_node)))

  # setting values for restructured nodes
  # resperennes0 became restemp0
  rm_names <- c("masec0", "QNplante0", "restemp0")
  set_param_value(
    xml_doc,
    param_name = as.list(rm_names),
    param_value = old_values
  )

  if (is.null(get_nodes(xml_doc, "//snow"))) {
    # Adding snow node
    new_node <- XML::xmlParseString(
      "<snow>
    <Sdepth0>0.0</Sdepth0>
    <Sdry0>0.0</Sdry0>
    <Swet0>0.0</Swet0>
    <ps0>0.0</ps0>
    </snow>",
      addFinalizer = TRUE
    )

    parent_node <- get_nodes(xml_doc, path = "//initialisations")[[1]]

    XML::addChildren(parent_node, XML::xmlClone(new_node))
  } else {
    # checking names an renaming them !
    old_names <- c("SDepth", "Sdry", "Swet", "ps")
    new_names <- c("Sdepth0", "Sdry0", "Swet0", "ps0")
    n <- get_nodes(xml_doc, c(sprintf("//%s", old_names)))

    if (!is.null(n)) {
      nodes_idx <- unlist(lapply(n, XML::xmlName)) %in% old_names
      n <- n[nodes_idx]
      new_names <- new_names[nodes_idx]
      for (i in seq_along(length(n))) {
        XML::xmlName(n[[i]]) <- new_names[i]
      }
    }
  }

  # Renaming soil parameters
  # hinit, NO3init, NH4init => hinitf, NO3initf, NH4initf
  current_node <- get_nodes(xml_doc, path = "//hinit")[[1]]
  XML::xmlName(current_node) <- "Hinitf"
  current_node <- get_nodes(xml_doc, path = "//NO3init")[[1]]
  XML::xmlName(current_node) <- "NO3initf"
  current_node <- get_nodes(xml_doc, path = "//NH4init")[[1]]
  XML::xmlName(current_node) <- "NH4initf"

  # Writing to file _ini.xml
  out_ini <- file.path(out_dir, basename(file))
  write_xml_file(xml_doc, out_ini, overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade a param_gen.xml file from STICS version 9 to 10
#'
#' @param file Path of a param_gen.xml file
#' @param out_dir Output directory path of the generated file
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @details See get_stics_versions_compat() for listing versions
#'
#' @examples
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_param_gen_xml_9_10(
#'   file = file.path(dir_path, "param_gen.xml"),
#'   out_dir = tempdir()
#' )
upgrade_param_gen_xml_9_10 <- function(
  file,
  out_dir,
  overwrite = FALSE
) {
  # Checking output directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Loading the old doc
  xml_doc <- xmldocument(file = file)

  # # Setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10.0"
  )

  # Nodes to remove
  rm_names <- c("FINERT", "FMIN1", "FMIN2", "FMIN3", "khaut", "rayon", "concrr")

  rm_nodes <- lapply(rm_names, function(x) {
    get_nodes(
      xml_doc,
      path = paste0("//param[@nom='", x, "']")
    )
  })
  lapply(rm_nodes, function(x) XML::removeNodes(x))

  # Nodes to change
  # <param format="real" max="20.0" min="1.0" nom="k_desat">3.0</param>
  # k_desat to kdesat
  nodes_to_change <- get_nodes(xml_doc, path = "//param[@nom='k_desat']")
  if (!is.null(nodes_to_change)) {
    set_attrs_values(
      xml_doc,
      path = "//param[@nom='k_desat']",
      attr_name = "nom",
      values_list = "kdesat"
    )
  }

  # Nodes to add
  new_node <- XML::xmlParseString(
    '<param format="real" max="1.0" min="0.0" nom="GMIN1">0.0007</param>
<param format="real" max="1.0" min="0.0" nom="GMIN2">0.02519</param>
<param format="real" max="1.0" min="0.0" nom="GMIN3">0.015</param>
<param format="real" max="1.0" min="0.0" nom="GMIN4">0.11200</param>
<param format="real" max="11.0" min="3.0" nom="GMIN5">8.50000</param>
<param format="real" max="1.0" min="0.0" nom="GMIN6">0.06000</param>
<param format="real" max="35.0" min="5.0" nom="GMIN7">11.00000</param>',
    addFinalizer = TRUE
  )

  new_nodes <- XML::getNodeSet(new_node, path = "//param")
  prev_sibling <- get_nodes(xml_doc, "//param[@nom='TREFr']")[[1]]

  # For adding them in the right order
  for (n in seq_along(new_nodes)) {
    new <- XML::xmlClone(new_nodes[[n]])
    XML::addSibling(prev_sibling, new)
    prev_sibling <- new
  }

  # Writing to file param_gen.xml
  write_xml_file(
    xml_doc,
    file.path(out_dir, basename(file)),
    overwrite = overwrite
  )

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade a param_newform.xml file from STICS version 9 to 10
#'
#' @param file Path of a param_newform.xml file
#' @param out_dir Output directory path of the generated file
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples

#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_param_newform_xml_9_10(
#'   file = file.path(dir_path,"param_newform.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
#'
upgrade_param_newform_xml_9_10 <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  # TODO: eliminate when option will be reactivated later.
  codemineral <- FALSE

  # Checking output directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Loading the old doc
  xml_doc <- xmldocument(file = file)

  # # Setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10.0"
  )

  # TODO : add from here if cond for calling specific version ranges updates

  # nodes to remove
  form_names <- c(
    "Specificities of cut crops",
    "Activation of the module simulating tiller dynamics",
    "Calculation of the maximal reserve compartment during reproductive stages",
    "Calculation of the stem elongation stage for perenial grasslands",
    "Moisture test for sowing decision",
    paste0(
      "automatic irrigations (associated with the options of automatic ",
      "irrigation in tec file)"
    ),
    "calculation of the root death at cutting date for grasslands",
    "option for several thinning ",
    "option for several fertilizer type ",
    # useless, options now removed
    "residue incorporation"
  )

  nodes_to_rm <- lapply(form_names, function(x) {
    get_nodes(
      xml_doc,
      path = paste0("//formalisme[@nom='", x, "']")
    )
  })

  lapply(nodes_to_rm, function(x) if (!is.null(x)) XML::removeNodes(x))

  # options to be removed
  opt_names <- c(
    "New mineralization of soil organic matter "
  )

  nodes_to_rm <- lapply(opt_names, function(x) {
    get_nodes(
      xml_doc,
      path = paste0("//option[@nom='", x, "']")
    )
  })
  lapply(nodes_to_rm, function(x) XML::removeNodes(x))

  # roots
  new_node <- XML::xmlParseString(
    '<formalisme nom="New Roots">
    <param format="integer" max="1.0" min="0.0" nom="humirac">1</param>
  </formalisme>',
    addFinalizer = TRUE
  )

  prev_sibling <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='Mineralization models']"
  )[[1]]
  XML::addSibling(prev_sibling, XML::xmlClone(new_node), after = TRUE)

  # options to add
  # min, CsurN

  parent_node <- prev_sibling

  if (codemineral) {
    new_node <- list(
      XML::xmlParseString(
        '<option choix="1" nom="New mineralization model" nomParam="codemineral">
  <choix code="1" nom="no"/>
  <choix code="2" nom="new_minr"/>
  <choix code="3" nom="new_minh+new_minr"/>
</option>',
        addFinalizer = TRUE
      ),
      XML::xmlParseString(
        '<option choix="2" nom="CsurNsol dynamic"
      nomParam="code_CsurNsol_dynamic">
  <choix code="1" nom="yes"/>
  <choix code="2" nom="no"/>
</option>',
        addFinalizer = TRUE
      )
    )

    lapply(
      new_node,
      function(x) XML::addChildren(parent_node, XML::xmlClone(x))
    )
  } else {
    # if a version 10.0 file is retreated
    # codemineral option must be retreived for the moment

    codemineral_node <- get_nodes(
      xml_doc,
      path = paste0("//option[@nomParam='codemineral']")
    )

    if (!is.null(codemineral_node)) {
      XML::removeNodes(codemineral_node)
    }

    new_node <- XML::xmlParseString(
      '<option choix="2" nom="CsurNsol dynamic"
      nomParam="code_CsurNsol_dynamic">
  <choix code="1" nom="yes"/>
  <choix code="2" nom="no"/>
</option>',
      addFinalizer = TRUE
    )

    XML::addChildren(parent_node, XML::xmlClone(new_node))
  }

  # formalism modifications
  # replacing formalisme option @nom, choix
  set_attrs_values(
    xml_doc,
    path = "//option[@nomParam='codecalferti']",
    attr_name = "nom",
    values_list = "automatic calculation of fertilisation"
  )

  set_attrs_values(
    xml_doc,
    path = "//option[@nomParam='codetesthumN']",
    attr_name = "nom",
    values_list = paste0(
      "automatic N fertilisation (1 = based on rainfall",
      " 2 = based on soil water content)"
    )
  )

  set_attrs_values(
    xml_doc,
    path = "//option[@nomParam='codetesthumN']",
    attr_name = "choix",
    values_list = "1"
  )

  # TODO: see what to do for the future v10 version !
  # ---------------------------------------------------------------------------
  # ISOP specific option to temporarily add
  new_node <- XML::xmlParseString(
    '<formalisme nom="ISOP">
		<option choix="2" nom="activation of ISOP equations" nomParam="code_ISOP">
			<choix code="1" nom="yes">
			<option choix="2" nom="activation of legume fixation in grassland"
			nomParam="code_pct_legume">
				<choix code="1" nom="yes">
					<param format="real" max="1.0" min="0.0" nom="pct_legum">0.5</param>
				</choix>
				<choix code="2" nom="no"/>
			</option>
			</choix>
			<choix code="2" nom="no"/>
		</option>
   </formalisme>',
    addFinalizer = TRUE
  )

  prev_sibling <- get_nodes(
    xml_doc,
    path = "//formalisme[@nom='New Roots']"
  )[[1]]
  XML::addSibling(prev_sibling, XML::xmlClone(new_node))
  # ---------------------------------------------------------------------------

  # Writing to file param_newform.xml
  write_xml_file(
    xml_doc,
    file.path(out_dir, basename(file)),
    overwrite = overwrite
  )

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade a sols.xml file from STICS version 9 to 10
#'
#' @param file Path of a sols.xml file
#' @param out_dir Output directory path of the generated file
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_sols_xml_9_10(
#'   file = file.path(dir_path, "sols.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_sols_xml_9_10 <- function(
  file,
  out_dir,
  param_gen_file,
  overwrite = FALSE
) {
  # checking output directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Loading the old doc
  xml_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10.0"
  )

  # Checking if layer @nom are up to date (old @nom = horizon)
  tableau_noms <- unlist(get_nodes(xml_doc, "//tableau/@nom"))

  if (any(grep(pattern = "horizon", tableau_noms))) {
    new_names <- unlist(lapply(
      tableau_noms,
      function(x) gsub(pattern = "horizon(.*)", x, replacement = "layer\\1")
    ))
    set_attrs_values(xml_doc, "//tableau", "nom", new_names)
  }

  # Nodes to add
  new_node <- XML::xmlParseString(
    '<param format="real" max="1.0" min="0.0" nom="finert">0.65000</param>',
    addFinalizer = TRUE
  )
  # new_node <- XML::xmlParseString('<param nom="finert">0.65000</param>',
  #                           addFinalizer = TRUE)

  prev_sibling <- get_nodes(xml_doc, "//param[@nom='CsurNsol']")

  # added for compatibility with old misspelled parameters
  if (is.null(prev_sibling)) {
    prev_sibling <- get_nodes(xml_doc, "//param[@nom='csurNsol']")
    # updating nom attribute content
    set_attrs_values(
      xml_doc,
      path = "//param[@nom='csurNsol']",
      attr_name = "nom",
      values_list = "CsurNsol"
    )
  }

  for (n in seq_along(prev_sibling)) {
    XML::addSibling(prev_sibling[[n]], XML::xmlClone(new_node))
  }

  # writing sols.xml file
  write_xml_file(xml_doc, file.path(out_dir, basename(file)), overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}


#' Upgrade a usms.xml file from STICS version 9 to 10
#'
#' @param file Path of a usms.xml file
#' @param out_dir Output directory path of the generated file
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param obs_dir Directory path of the observation data files
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @details See get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_usms_xml_9_10(
#'   file = file.path(dir_path, "usms.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_usms_xml_9_10 <- function(
  file,
  out_dir,
  param_gen_file,
  obs_dir = NULL,
  overwrite = FALSE
) {
  # Checking output directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  if (is.null(obs_dir)) {
    obs_dir <- dirname(file)
  }

  # loading the old doc
  xml_doc <- xmldocument(file = file)

  # # setting file STICS version
  set_xml_file_version(
    xml_doc,
    new_version = "V10.0"
  )

  # checking if fobs exist
  obs_nodes <- get_nodes(xml_doc, "//fobs")

  # TODO: detect if fobs exist and evaluate
  # where to add fobs fields !!!!!
  # default behavior: no existing fobs fields
  if (is.null(obs_nodes)) {
    new_node <- XML::xmlParseString("<fobs>null</fobs>", addFinalizer = TRUE)

    parent_node <- get_nodes(xml_doc, "//plante")

    lapply(
      parent_node,
      function(x) XML::addChildren(x, XML::xmlClone(new_node))
    )
  }

  # Usms names
  usms_names <- get_attrs_values(xml_doc, "//usm", "nom")

  # existing obs files
  # intercrops usms are not taken into account in that case
  obs_names <- paste0(usms_names, ".obs")
  obs_exist <- file.exists(file.path(obs_dir, obs_names))
  obs_val <- rep("null", length(usms_names))
  obs_val[obs_exist] <- obs_names[obs_exist]

  # Setting obs files names into fobs for existing files
  set_param_value(
    xml_doc,
    param_name = "fobs",
    param_value = obs_val,
    parent_name = "plante",
    parent_sel_attr = "1"
  )

  # writing file
  write_xml_file(xml_doc, file.path(out_dir, basename(file)), overwrite)

  XML::free(xml_doc@content)
  invisible(gc(verbose = FALSE))
}

#' Upgrade XML files of a JavaSTICS V9 workspace directory
#'
#' @param workspace Path of a JavaSTICS workspace
#' @param javastics Path of JavaSTICS containing the STICS version corresponding
#' to the version of the files to be converted
#' @param out_dir Output directory of the generated files
# @param stics_version Name of the STICS version (VX.Y format)
# @param target_version Name of the STICS version to upgrade files
# to  (VX.Y format)
#' @param plant logical (optional), TRUE for upgrading plant files if a "plant"
#' sub-directory of workspace exists, FALSE otherwise
#' @param overwrite logical (optional),
#' TRUE for overwriting files if they exist, FALSE otherwise
#' @param verbose   logical, TRUE for displaying a copy message
#' FALSE otherwise (default)
# @param ... Additional input arguments
#'
#' @return None
#'
#' @export
#'
#' @details
#' - See SticsRFiles::get_stics_versions_compat() for listing versions
#' - If general parameters files exist in `workspace`, they are also upgraded.
#' In that case, residues parameters values are kept and might not be adapted
#' to the target model version.
#' - Weather data and observations files are fully copied to `out_dir`
#'
#' @examples
#' \dontrun{
#' upgrade_workspace_xml_9_10(
#'   workspace = "/path/to/JavaSTICS/workspace",
#'   javastics = "/path/to/JavaSTICS/folder",
#'   out_dir = "/path/to/an/output/directory"
#' )
#' }
upgrade_workspace_xml_9_10 <- function(
  workspace,
  javastics,
  out_dir,
  plant = FALSE,
  overwrite = FALSE,
  verbose = FALSE
) {
  # Getting param_gen.xml path
  par_gen <- get_param_gen_file(
    file_name = "param_gen.xml",
    workspace,
    javastics
  )

  stics_version <- get_xml_file_version(par_gen)

  # Just in case, creating the target directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  if (verbose) {
    message(
      paste(
        "Upgrading files from version",
        stics_version,
        "to",
        "V10",
        "\n"
      ),
      paste("From: ", workspace, "\n"),
      paste("To: ", out_dir, "\n"),
      "-----------------------------------\n"
    )
  }

  # Converting param_gen.xml
  upgrade_param_gen_xml_9_10(
    file = par_gen,
    out_dir = out_dir,
    overwrite = overwrite
  )

  if (verbose) {
    message("param_gen.xml\n")
  }
  # }

  # Getting param_newform.xml path
  par_new <- get_param_gen_file(
    file_name = "param_newform.xml",
    workspace,
    javastics
  )

  # Converting param_newform.xml
  upgrade_param_newform_xml_9_10(
    file = par_new,
    out_dir = out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )

  if (verbose) {
    message("param_new_form.xml\n")
  }

  # Converting usms.xml file
  usms <- file.path(workspace, "usms.xml")

  upgrade_usms_xml_9_10(
    file = usms,
    out_dir = out_dir,
    param_gen_file = par_gen,
    obs_dir = workspace,
    overwrite = overwrite
  )

  if (verbose) {
    message("usms.xml\n")
  }

  # Converting sols.xml file
  sols <- file.path(workspace, "sols.xml")

  upgrade_sols_xml_9_10(
    file = sols,
    out_dir = out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )

  if (verbose) {
    message("sols.xml\n")
  }

  # Converting station files (*_sta.xml)
  sta_files <- get_in_files(in_dir_or_files = workspace, kind = "sta")

  upgrade_sta_xml_9_10(
    file = sta_files,
    out_dir = out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )

  if (verbose) {
    message("*_sta.xml\n")
  }

  # Converting initialisation files (*_ini.xml)
  ini_files <- get_in_files(in_dir_or_files = workspace, kind = "ini")

  upgrade_ini_xml_9_10(
    file = ini_files,
    out_dir = out_dir,
    param_gen_file = par_gen,
    overwrite = overwrite
  )

  if (verbose) {
    message("*_ini.xml\n")
  }

  # Converting crop management files (*_tec.xml)
  tec_files <- get_in_files(in_dir_or_files = workspace, kind = "tec")

  upgrade_tec_xml_9_10(
    file = tec_files,
    out_dir = out_dir,
    param_newform_file = par_new,
    param_gen_file = par_gen,
    overwrite = overwrite
  )

  if (verbose) {
    message("*_tec.xml\n")
  }

  # Upgrading plant files
  # if a plant sub directory exists in workspace
  plant_files <- get_in_files(
    in_dir_or_files = file.path(workspace, "plant"),
    kind = "plt"
  )

  if (length(plant_files) > 0) {
    # For creating a sub-directory in workspace for upgraded plant files
    plant_out_dir <- file.path(out_dir, "plant")
    if (!dir.exists(plant_out_dir)) {
      dir.create(plant_out_dir)
    }

    upgrade_plt_xml_9_10(
      file = plant_files,
      out_dir = plant_out_dir,
      param_gen_file = par_gen,
      param_newform_file = par_new,
      overwrite = overwrite
    )

    if (verbose) {
      message("*_plt.xml\n")
    }
  }

  # TODO: see how to manage variables names checks in *.mod files
  # Probably, the new JavaSTICS path may be added as a new function argument
  # for getting information on output variables
  # (use get_var_info with the appropriate version string)

  workspace_files_copy(
    workspace = workspace,
    javastics = javastics,
    out_dir = out_dir,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {
    message(paste0(
      "-----------------------------------\n",
      "Files upgrade and copy is complete.\n"
    ))
  }
}
