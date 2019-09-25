#' @title Getting parameter values from xml files
#'
#' @description Extracting parameter values for a list of xmlDocument and
#' of parameters
#'
#' @param xml_files an xml file, or a vector/list of
#' @param param_names_list parameter names list (i.e.: option parameter name, parameter name,
#' other kinds in ini and some tec parameters not taken into account for the moment)
#'
#' @return A list of parameter list length, with parameters values for each xml file
#'
#' @examples
#' get_files_params_values(path/to/xml/file,"codetemp")
#' get_files_params_values(c(path/to/xml/file1,path/to/xml/file2),"codetemp")
#' get_files_params_values(path/to/xml/file,c("codetemp","codegdh"))
#' get_files_params_values(c(path/to/xml/file1,path/to/xml/file2),c("codetemp","codegdh"))
#'
#' @export
#'
# ----------------------------------------------------------------------
#  MODIFICATIONS (last commit)
#  $Date: 2019-09-24 17:57:18 +0200 (mar. 24 sept. 2019) $
#  $Author: sbuis $
#  $Revision: 1598 $
# ----------------------------------------------------------------------

get_files_params_values <- function(xml_files,param_names_list,
                              parent_name = NULL, parent_sel_attr = NULL){


  xml_docs <- lapply(xml_files,xmldocument)

  values <- get_param_value(xml_docs, param_name = param_names_list,
                              parent_name = parent_name,
                              parent_sel_attr = parent_sel_attr)


  return(values)


}
