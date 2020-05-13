#' @title Getting parameter values from xml files
#'
#' @description Extracting parameter values for a list of xml files and parameters
#'
#' @param xml_file An xml file, or a vector/list of
#' @param param_name parameter names vector (i.e.: parameter name or option code, optional)
#' @param select node name or attribute name to use for selection (optional, default to no selection)
#' @param value value used for select (optional)
#' @param ... Pass further arguments to `get_param_value()`
#'
#' @return A list of parameter values for each xml_file (a list of list)
#'
#' @examples
#' \dontrun{
#' # Soil file
#' xml_path = file.path(get_examples_path( file_type = "xml"),"sols.xml")
#'
#' # For all soils
#' get_param_xml(xml_path)
#' get_param_xml(xml_path, "argi")
#' get_param_xml(xml_path, c("argi", "norg"))
#'
#' # For one soil selection
#' get_param_xml(xml_path, "argi",
#' select = "sol", value = "solcanne")
#'
#' # For soils and parameters vectors
#' # scalar parameters per soil
#' get_param_xml(xml_path, c("argi", "norg"),
#' select = "sol", value = c("solcanne", "solbanane"))
#'
#' $sols.xml
#' $sols.xml$argi
#' [1] 30.2 70.0
#'
#' $sols.xml$norg
#' [1] 0.27 0.20
#'
#' # vector parameters per soil (5 values, one per soil layer)
#' get_param_xml(xml_path, c("epc", "HCCF"),
#' select = "sol", value = c("solcanne", "solbanane"))
#'
#' $sols.xml
#' $sols.xml$epc
#' [1] 20 20 20 20 20 10 10 10 10 40
#'
#' $sols.xml$HCCF
#' [1] 46.8 46.4 48.5 50.1 50.1 41.0 41.0 41.0 42.0 42.0
#'
#' # Crop management file
#' xml_path = file.path(get_examples_path( file_type = "xml"),"file_tec.xml")
#'
#' get_param_xml(xml_path)
#'
#' # Getting parameters for irrigation (date and quantity)
#' get_param_xml(xml_path, c("julapI_or_sum_upvt", "amount"))
#'
#'
#' # Getting all parameters for a given formalism: "irrigation"
#' get_param_xml(xml_path, select = "formalisme", value = "irrigation")
#'
#' }
#' @export
get_param_xml <- function(xml_file,
                          param_name=NULL,
                          select = NULL,
                          value = NULL,
                          ...) {
  # ... argument for passing : ids, show_xpath to get_param_value


  xml_docs <- lapply(xml_file,xmldocument)

  values <- get_param_value(xml_doc = xml_docs,
                            param_name = param_name,
                            parent_name = select,
                            parent_sel_attr = value,
                            ...)
  xml_names= lapply(xml_file, basename)%>%unlist()

  # If there are duplicated names in xml_file:
  is_duplicated_name= xml_names%>%duplicated()
  xml_names[is_duplicated_name]= paste0("xml_",which(is_duplicated_name==TRUE),"_",xml_names[is_duplicated_name])

  names(values) <- xml_names

  return(values)
}
