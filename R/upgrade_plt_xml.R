#' Upgrading _plt.xml file(s) to a newer version
#'
#' @param file Path of an initialisation (*_plt.xml) file or a vector of
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param param_newform_file Path of the param_newform.xml file corresponding
#' to the file version
#' @param out_dir Output directory path of the generated files
#' @param stics_version Name of the Stics version (VX.Y format)
#' @param target_version Name of the Stics version to upgrade files to (VX.Y format)
#' @param check_version Perform version consistency with in stics_version input
#' with the file version and finally checking if the upgrade is possible
#' allowed to the target_version. If TRUE, param_gen_file is mandatory.
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
#' upgrade_plt_xml(file = "/path/to/_plt.xml",
#'                 param_gen_file = "/path/to/param_gen.xml",
#'                 param_newform_file = "/path/to/param_newform.xml"
#'                 out_dir = "/path/to/directory")
#' }
upgrade_plt_xml <- function(file,
                            param_gen_file,
                            param_newform_file,
                            out_dir,
                            stics_version = "V9.2",
                            target_version = "V10.0",
                            check_version = TRUE,
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

    min_version <- SticsRFiles:::get_version_num("V9.1")

    # extracting or detecting the Stics version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- SticsRFiles:::check_xml_file_version(file[1],
                                                         stics_version,
                                                         param_gen_file = param_gen_file)


    if(!file_version && is.null(param_gen_file))
      stop("param_gen_file must be provided! ")

    if (!file_version) {
      stop("The input version ",stics_version,
           " does not match file version ",
           attr(file_version,"version")," \n",file[1] )
    }

    # Compatibility checks between version and update to target_version
    ver_num <- SticsRFiles:::get_version_num(stics_version)
    if (ver_num < min_version)
      stop("Files from the version ", stics_version,
           " cannot be converted to the version ", target_version)


    # for checking only once when multiple files are treated !
    check_version <- FALSE
  }


  # Treating a files list
  if (length(file) > 1 ) {
    lapply(file, function(x) upgrade_plt_xml(file = x,
                                             out_dir = out_dir,
                                             param_gen_file = param_gen_file,
                                             param_newform_file = param_newform_file,
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
  old_doc <- SticsRFiles:::xmldocument(file = file)
  #old_param_doc <- SticsRFiles:::xmldocument(file = param_gen_file)

  # Setting file stics version
  SticsRFiles:::set_xml_file_version(old_doc, new_version = target_version, overwrite = overwrite)




  #--------------------------------------------------------------------------------------------------------
  # Getting patameters to set to varietal ones
  # Parameters names to move to varietal parameters
  # coming from other formalisms
  param_names_varietal <- c("phobase", "phosat", "stdordebour", "bdens", "hautbase", "hautmax", "dlaimax","dlaimaxbrut",
                            "innsen", "rapsenturg", "extin", "ktrou", "temin", "teopt", "slamax", "tigefeuil",
                            "nbjgrain", "nbgrmin", "vitircarb", "vitircarbT",
                            "stdrpnou", "nbinflo", "inflomax", "pentinflores",
                            "vitpropsucre", "vitprophuile", "vitirazo",
                            "tgellev10", "tgeljuv10", "tgelveg10",
                            "psisto", "psiturg",
                            "deshydbase" )
  param_values_varietal <- SticsRFiles:::get_param_value(old_doc, param_names_varietal)

  nodes_to_rm <- lapply(param_names_varietal, function(x)
    SticsRFiles:::getNodeS(docObj = old_doc,
                           path = paste0("//param[@nom='",x,"']")))
  lapply(nodes_to_rm, function(x) removeNodes(x))

  # parameters names already existing in varietal parameters
  param_names_keep <- c(
    "stlevamf", "stamflax", "stlevdrp", "stflodrp", "stdrpdes", "pgrainmaxi", "adens", "croirac", "durvieF",
    "jvc", "sensiphot","stlaxsen", "stsenlan", "nbgrmax", "stdrpmat", "afruitpot", "dureefruit")

  param_values_keep <- SticsRFiles:::get_param_value(old_doc, param_names_keep)




  # Specific to plants
  new_node <- xmlParseString(
    '<option choix="2" nom="effect of decreasing photoperiod on biomass allocation" nomParam="codephot_part">
    <choix code="1" nom="yes"/>
    <choix code="2" nom="no"/>
    </option>')

  parent_node <- SticsRFiles:::getNodeS(old_doc, '//option[@nomParam="codephot"]/choix[@code="1"]')[[1]]
  addChildren(parent_node, xmlClone(new_node))

  # move
  # <param format="integer" max="10" min="0" nom="nbfeuilplant">0</param>
  node_to_move <- SticsRFiles:::getNodeS(old_doc, path="//param[@nom='nbfeuilplant']")[[1]]
  prev_sibling <- SticsRFiles:::getNodeS(old_doc, path="//param[@nom='laiplantule']")[[1]]

  addSibling(prev_sibling, node_to_move)

  # ---------------------------------------------------
  # TODO: see irmax to move and irazomax to add !!!!!!
  # --------------------------------------------------------

  # ------------------------
  # TODO: add codedisrac
  new_node <- xmlParseString(
    '<option choix="2" nom="Standard root distribution" nomParam="codedisrac">
    <choix code="1" nom="yes">
    <param format="real" max="0.01" min="0.00001" nom="kdisrac">0.00158</param>
    <param format="real" max="1.0" min="0.0" nom="alloperirac">0.23</param>
    </choix>
    <choix code="2" nom="no"/>
    </option>',
    addFinalizer = TRUE)
  # before sibling <option choix="2" nom="N effect on root distribution" nomParam="codazorac">
  prev_sibling <- SticsRFiles:::getNodeS(old_doc, '//option[@nomParam="codazorac"]')[[1]]
  addSibling(prev_sibling, new_node, after = FALSE)
  # ----------------------------------------------------------


  # Changing varietal Structure
  # Remove nodes from old doc for varietal content (under variete nodes)
  nodes_to_rm <- SticsRFiles:::getNodeS(old_doc, path="//variete/*")
  lapply(nodes_to_rm, function(x) removeNodes(x))

  # Add under each variete node new nodes
  var_nodes <- xmlParseString(
    '<formalismev nom="phenological stages">
			<param format="real" max="6000.0" min="0.0" nom="stlevamf">275</param>
			<param format="real" max="6000.0" min="0.0" nom="stamflax">375</param>
			<param format="real" max="6000.0" min="0.0" nom="stlevdrp">837</param>
			<param format="real" max="500.0" min="0.0" nom="stflodrp">0</param>
			<param format="real" max="900.0" min="0.0" nom="stdrpdes">700</param>
			<optionv nom="codebfroid">
				<param code="2" format="integer" max="70" min="0" nom="jvc">55</param>
				<param code="3" format="real" max="20000.0" min="0.0" nom="stdordebour">0</param>
			</optionv>
			<optionv nom="codephot">
				<param code="1" format="real" max="1.0" min="0.0" nom="sensiphot">0</param>
				<param code="1" format="real" max="24.0" min="0.0" nom="phobase">6.3</param>
				<param code="1" format="real" max="24.0" min="0.0" nom="phosat">20</param>
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
				<param code="1" format="real" max="6000.0" min="0.0" nom="stlaxsen">675.</param>
				<param code="1" format="real" max="6000.0" min="0.0" nom="stsenlan">110.</param>
				<param code="1" format="real" max="0.5" min="5.0E-6" nom="dlaimax">0.00047</param>
				<param code="2" format="real" max="0.5" min="5.0E-6" nom="dlaimaxbrut">0.00047</param>
				<param code="2" format="real" max="1.0" min="-2.0" nom="innsen">0.17</param>
				<param code="2" format="real" max="1.5" min="0.5" nom="rapsenturg">0.50</param>
			</optionv>
</formalismev>
<formalismev nom="radiation interception">
			<optionv nom="codetransrad">
				<param code="1" format="real" max="1.5" min="0.1" nom="extin">0.50</param>
				<param code="2" format="real" max="2.0" min="0.1" nom="ktrou">1.00</param>
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
			<param format="real" max="5.0" min="0.0" nom="pgrainmaxi">0.03970</param>
			<param format="real" max="0.01" min="0.0010" nom="vitpropsucre">0.00000</param>
			<param format="real" max="0.01" min="0.0010" nom="vitprophuile">0.00000</param>
			<param format="real" max="0.04" min="0.0010" nom="vitirazo">0.01450</param>
		    <param format="real" max="0.02" min="-0.02" nom="deshydbase">0.008000</param>
			<optionv nom="codeindetermin">
				<param code="1" format="integer" max="40" min="5" nom="nbjgrain">30</param>
				<param code="1" format="real" max="10000.0" min="0.0" nom="nbgrmin">6000</param>
				<param code="1" format="real" max="1000000.0" min="0.0" nom="nbgrmax">30000</param>
				<param code="1" format="real" max="2000.0" min="0.0" nom="stdrpmat">700</param>
				<optionv code="1" nom="codeir">
					<param code="1" format="real" max="0.02" min="0.001" nom="vitircarb">0.0107</param>
					<param code="2" format="real" max="0.002" min="5.0E-5" nom="vitircarbT">0</param>
				</optionv>
				<param code="2" format="real" max="20.0" min="0.5" nom="afruitpot">.00000</param>
				<param code="2" format="real" max="2000.0" min="10.0" nom="dureefruit">.00000</param>
				<param code="2" format="real" max="6000.0" min="0.0" nom="stdrpnou">-999</param>
				<optionv code="1" nom="codcalinflo">
					<param code="1" format="real" max="100.0" min="0.0" nom="nbinflo">-999</param>
				    <param code="2" format="real" max="100.0" min="0.0" nom="inflomax">-999</param>
					<param code="2" format="real" max="10.0" min="0.0" nom="pentinflores">-999</param>
				</optionv>
			</optionv>
</formalismev>
<formalismev nom="roots">
			<param format="real" max="0.5" min="0.0" nom="croirac">0.12</param>
</formalismev>
<formalismev nom="frost">
			<optionv nom="codgellev">
				<param code="2" format="real" max="0.0" min="-25.0" nom="tgellev10">-4.0</param>
			</optionv>
			<optionv nom="codgeljuv">
				<param code="2" format="real" max="0.0" min="-25.0" nom="tgeljuv10">-10.0</param>
			</optionv>
			<optionv nom="codgelveg">
				<param code="2" format="real" max="0.0" min="-25.0" nom="tgelveg10">-4.5</param>
			</optionv>
</formalismev>
<formalismev nom="water stress">
			<param format="real" max="25.0" min="1.0" nom="psisto">15</param>
			<param format="real" max="15.0" min="1.0" nom="psiturg">4</param>
			<param format="real" max="0.3" min="0.05" nom="swfacmin">0.1</param>
</formalismev>',
    addFinalizer = TRUE)


  # adding var nodes under each "variete" node
  var_par_nodes <- SticsRFiles:::getNodeS(old_doc, path="//variete")
  lapply(var_par_nodes, function(x) addChildren(x, kids = xmlChildren(xmlClone(var_nodes))))

  #
  # Set values of all kept parameter values (removed nodes or moved) to new nodes
  # param_values_varietal
  SticsRFiles:::set_param_value(old_doc, param_names_varietal, param_values_varietal)
  SticsRFiles:::set_param_value(old_doc, param_names_keep, param_values_keep)



  # ----------------------------------------------------------------------------------------
  # Adding new nodes
  #
  # codemortalracine
  # after <option choix="1" nom="driving temperature" nomParam="codetemprac">
  new_node  <- xmlParseString(
    '<option choix="2" nom="calculation of the root death at cutting date for grasslands" nomParam="codemortalracine">
	<choix code="1" nom="function of dry matter production between two successives cut">
		<param format="real" max="1.0" min="0.0" nom="coefracoupe">0.5</param>
	</choix>
    <choix code="2" nom="no specific root death at cutting"/>
</option>',
    addFinalizer = TRUE)

  prev_sibling <- SticsRFiles:::getNodeS(old_doc, "//*[@nomParam='codetemprac']")[[1]]
  addSibling(prev_sibling, xmlClone(new_node))


  # code_WangEngel
  # after <choix code="1" nom="daily temperatures">
  new_node  <- xmlParseString(
    '<option choix="2" nom=" Wang et Engel (1998)" nomParam="code_WangEngel">
	<choix code="1" nom="Wang et Engel">
		<param format="real" max="30" min="0" nom="tdoptdeb">11.7</param>
	</choix>
	<choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE)

  par_node <- SticsRFiles:::getNodeS(old_doc, "//*[@nomParam='codegdhdeb']/choix[@code='1']")[[1]]
  addChildren(par_node, new_node)


  # In <formalisme nom="partitioning of biomass in organs">
  # after
  # <param format="real" max="300.0" min="0.0" nom="sea">100.00000</param>
  new_nodes <- xmlParseString(
    '<option choix="2" nom="Simulation of Nitrogen and Carbon reserves" nomParam="code_acti_reserve">
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
    <option choix="2" nom="Calculation of the stem elongation stage for perenial grasslands" nomParam="codemontaison">
		<choix code="1" nom="yes"/>
		<choix code="2" nom="no"/>
</option>
<option choix="2" nom="Simulation of tiller dynamics for grasslands" nomParam="codedyntalle">
	<choix code="1" nom="yes">
		<option choix="1" nom="Choice of the ratio used to calculate tiller mortality" nomParam="codetranspitalle">
			<choix code="1" nom="et/etm"/>
			<choix code="2" nom="ep/eop"/>
		</option>
			<param format="real" max="0.00001" min="0.0000025" nom="SurfApex">0.000005</param>
			<param format="real" max="0.04" min="0.01" nom="SeuilMorTalle">0.02</param>
			<param format="real" max="0.2" min="0.1" nom="SigmaDisTalle">0.1</param>
			<param format="real" max="0.01" min="0.00001" nom="VitReconsPeupl">0.001</param>
			<param format="real" max="1200" min="400" nom="SeuilReconsPeupl">800</param>
			<param format="real" max="5000" min="3000" nom="MaxTalle">4000.0</param>
			<param format="real" max="2.0" min="0.5" nom="SeuilLAIapex">1.0</param>
			<param format="real" max="10.0" min="1.0" nom="tigefeuilcoupe">5.0</param>
	</choix>
	<choix code="2" nom="no"/>
</option>',
    addFinalizer = TRUE)

  par_node <- SticsRFiles:::getNodeS(old_doc,
                                     "//formalisme[@nom='partitioning of biomass in organs']")[[1]]
  addChildren(par_node, kids = unlist(xmlChildren(new_nodes)))


  # <formalisme nom="roots">
  # rayon:   => DONE
  #
  new_node  <- xmlParseString('<param format="real" max="0.07" min="0.005" nom="rayon">0.02</param>')
  prec_sibling <-  SticsRFiles:::getNodeS(old_doc, "//*[@nom='contrdamax']")[[1]]
  addSibling(prec_sibling, new_node)

  #
  new_nodes <- xmlParseString(
    '<option choix="2" nom="Preferential allocation of biomass to roots in case of water or N stress" nomParam="code_stress_root">
    <choix code="1" nom="yes"/>
    <choix code="2" nom="no"/>
    </option>
    <option choix="2" nom="rootdeposition activation" nomParam="code_rootdeposition">
    <choix code="1" nom="yes">
    <param format="real" max="60.0" min="10.0" nom="parazorac">-999</param>
    <option choix="2" nom="simulation of 2 roots classes" nomParam="code_diff_root">
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
    addFinalizer = TRUE)

  par_node <- SticsRFiles:::getNodeS(old_doc,"//choix[@nom='true density']")[[1]]

  addChildren(par_node, kids = unlist(xmlChildren(new_nodes)))


  # -----------------------------------------------------------
  # Update param values
  #
  # from param_gen.xml
  # khaut, rayon
  param_gen_values <- SticsRFiles:::get_param_xml(xml_file = param_gen_file,
                                                  param_name = c("rayon", "khaut"))[[1]]
  SticsRFiles:::set_param_value(old_doc, param_name = c("rayon", "khaut"), param_value = param_gen_values)


  # from param_newform.xml
  #
  # coefracoupe(1), coefracoupe(2) -> coefracoupe
  param_newform_values <- SticsRFiles:::get_param_xml(xml_file = param_newform_file,
                                                  param_name = c("coefracoupe(1)", "coefracoupe(2)"))[[1]]
  if (length(unique(unlist(param_newform_values))) > 1) stop("Multiple values of coefracoupe in param_gen.xml file")
  SticsRFiles:::set_param_value(old_doc, param_name = "coefracoupe", param_value = param_newform_values[[1]])


  # Updating other values than nodes values (i.e. nodes attributes values)
  # TODO
  # Changing param min / max attributes values
  # hautbase => 0.1


  # Changing options' "choix",  "nom" attribute values
  #
  # oui to yes, non to no
  nodes_to_change <- SticsRFiles:::getNodeS(old_doc, path="//choix[@nom='oui']")
  if (!is.null(nodes_to_change))
    setAttrValues(old_doc, path="//choix[@nom='oui']", attr_name="nom", values_list = "yes")
  nodes_to_change <- SticsRFiles:::getNodeS(old_doc, path="//choix[@nom='non']")
  if (!is.null(nodes_to_change))
    setAttrValues(old_doc, path="//choix[@nom='non']", attr_name="nom", values_list = "no")




  # Writing to file _plt.xml
  out_plt <- file.path(out_dir, basename(file))
  SticsRFiles:::write_xml_file(old_doc, out_plt, overwrite)



  free(old_doc@content)
  invisible(gc(verbose = FALSE))



  return()



}
