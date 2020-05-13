#' Extracting a data.frame of parameters values for an xml file or a set of (whatever the xml file kind)
#'
#' @param file_path A file path or a vector of
#' @param select node name or attribute name to use for selection (optional, default to no selection)
#' @param name value used for select (optional)
#' @param param_names vector of parameters names (optional)
#'
#' @return A data.frame with name, type, param, id and value columns
#'
#'
#' @details An extract of an example of data.frame
#'
#' |name          |type       |param     | id|value            |
#' |:-------------|:----------|:---------|--:|:----------------|
#' |SugarCane     |usms       |datedebut | NA|286              |
#' |SugarCane     |usms       |datefin   | NA|650              |
#' |SugarCane     |usms       |finit     | NA|canne_ini.xml    |
#' |SugarCane     |usms       |nomsol    | NA|solcanne         |
#' |SugarCane     |usms       |fstation  | NA|climcanj_sta.xml |
#' |...           |...        |...       |...|...              |
#' |...           |...        |...       |...|...              |
#' |param_gen.xml |fichierpar |Qmulchdec | 17|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 18|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 19|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 20|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 21|0                |
#'
#' name: a file name or an usm or soil name
#' type: type of file
#' param: param name
#' id: NA for scalar, integer id for vectors parameters
#' value: parameter value
#'
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#' dir_path <- get_examples_path("xml")
#' get_xml_files_param_df(file_path = file.path(dir_path,"sols.xml"))
#'
#' get_xml_files_param_df(file_path = file.path(dir_path,"sols.xml"),
#' select = "sol", c("solcanne", "solble"))
#'
#' files_list <- file.path(dir_path, c("sols.xml", "usms.xml", "param_gen.xml" ))
#' get_xml_files_param_df(file_path = files_list)
#'
#' }
#'
#'
get_xml_files_param_df <- function(file_path, select = NULL, name = NULL, param_names = NULL) {


  # for managing a files list
  if (length(file_path) > 1) {
    files_exist <- file.exists(file_path)

    if (!all(files_exist)) warning("Missing files: ", paste(file_path[!files_exist]), collapse=",")

    df <- data.table::rbindlist(lapply(file_path[files_exist],
                                       function(x) get_xml_files_param_df(x,
                                                                         select = select,
                                                                         name = name,
                                                                         param_names = param_names)))
    return(df)
  }


  # Checking if select can be set (for sols and usms)
  if (base::is.null(select)) {
    if (is_sols_xml(file_path)) select = "sol"
    if (is_usms_xml(file_path)) select = "usm"
  }

  # Getting usm or sol names vector
  names_list <- NULL
  if (!base::is.null(select)) names_list <- get_param_xml(file_path, param_name = select)[[1]]

  # Getting all usm or sol names from the file
  if (is.null(name)) {
    name <- names_list
  } else {
    # Checking names
    exist_names <- name %in% names_list
    if (! all(exist_names)) {
      stop("Missing names in file: ", paste(name[!exist_names], collapse = ","))
    }
  }

  # For a vector of usm or sol names
  if (length(name) > 1) {
    return(data.table::rbindlist(lapply(name,
                                        function(x) get_xml_files_param_df(file_path = file_path,
                                                                          select = select,
                                                                          name = x,
                                                                          param_names = param_names))))
  }

  # Getting full param names list
  all_param_names <- get_param_names_xml(file_path)$name

  # Getting parameters names vector
  # Without or with selection against param_names list
  if (base::is.null(param_names)) {
    param_names <- all_param_names
  } else {
    param_names <- all_param_names[ all_param_names %in% param_names ]
  }

  # for one name
  param_values <- get_param_xml(file_path, param_names = param_names, select = select, value = name)[[1]]

  # Getting parameters values number
  values_nb <- unlist(lapply(X = param_values, function(x) length(x)))

  # Getting expanded parameters names vector
  param <-  rep(names(param_values), values_nb)

  # Calculating identifier for each occurrence of a parameter
  id <- unlist(lapply(values_nb, function(x)  {l <- NA; if (x > 1) l <- 1:x; return(l)}), use.names = FALSE)

  if (base::is.null(name)) {
    name_col <- rep(basename(file_path), sum(values_nb))
  } else {
    name_col <- rep(name, sum(values_nb))
  }

  # Setting file type
  type_col <- rep(get_xml_type(file_path), sum(values_nb))

  # Defining the returned data.frame
  data_df <- data.frame(name = name_col,
                        type = type_col,
                        param = param,
                        id = id,
                        value=unlist(param_values, use.names = F),
                        stringsAsFactors = FALSE)
  return(data_df)
}
