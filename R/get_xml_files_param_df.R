#' Extracting a data.frame of parameters values for an xml file or a set of (whatever the xml file kind)
#'
#' @param file_path A file path or a vector of
#' @param select node name or attribute name to use for selection (optional, default to no selection)
#' @param name value used for select (optional)
#' @param param_names vector of parameters names (optional)
#' @param wide_shape Optional logical for keeping the long data.frame format FALSE, default)
#' or converting it to a wider format one (TRUE)
#'
#' @return A data.frame with name, type, param, id and value columns
#'
#'
#' @details An extract of an example of data.frame for the 2 formats
#'
#'  * Default one
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
#' * wide format
#'
#'  |name      |type | datedebut| datefin|finit          |nomsol    |fstation         |fclim1        |fclim2        |
#'  |:---------|:----|---------:|-------:|:--------------|:---------|:----------------|:-------------|:-------------|
#'  |SugarCane |usms |       286|     650|canne_ini.xml  |solcanne  |climcanj_sta.xml |climcanj.1998 |climcanj.1999 |
#'  |potato    |usms |        91|     250|patate_ini.xml |solpatate |climpdtj_sta.xml |climpdtj.1997 |climpdtj.1997 |
#'  |banana    |usms |        30|     300|banane_ini.xml |solbanane |climbanj_sta.xml |climbanj.1996 |climbanj.1996 |
#'  |sorghum   |usms |       112|     360|sorgho_ini.xml |solsorgho |climsorj_sta.xml |climsorj.1996 |climsorj.1996 |
#'  |barley    |usms |        88|     196|orge_ini.xml   |solorge   |climorgj_sta.xml |climorgj.2002 |climorgj.2002 |
#'
#' name: a file name or an usm or soil name
#' type: type of file
#' all other columns names correspond to parameter names, with indices as suffix
#' for multiple values
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' dir_path <- get_examples_path("xml")
#' get_xml_files_param_df(file_path = file.path(dir_path, "sols.xml"))
#'
#' get_xml_files_param_df(
#'   file_path = file.path(dir_path, "sols.xml"),
#'   select = "sol", c("solcanne", "solble")
#' )
#'
#' files_list <- file.path(dir_path, c("sols.xml", "usms.xml", "param_gen.xml"))
#' get_xml_files_param_df(file_path = files_list)
#' }
#'
get_xml_files_param_df <- function(file_path, select = NULL, name = NULL, param_names = NULL, wide_shape = FALSE) {


  # For managing a files list
  if (length(file_path) > 1) {
    files_exist <- file.exists(file_path)

    if (!all(files_exist)) warning("Missing files: ", paste(file_path[!files_exist], collapse = ","))

    if (!any(files_exist)) stop("Not any files exist !")

    files_df <- lapply(
      file_path[files_exist],
      function(x) {
        get_xml_files_param_df(x,
          select = select,
          name = name,
          param_names = param_names,
          wide_shape = FALSE
        )
      }
    )

    df <- data.table::rbindlist(files_df)

    # Conversion to a wider table (with type conversion)
    if (wide_shape) {
      df <- df_wider(df)
    }

    return(df)
  }


  # Checking if select can be set (for sols and usms)
  if (base::is.null(select)) {
    if (is_sols_xml(file_path)) select <- "sol"
    if (is_usms_xml(file_path)) select <- "usm"
  }

  # Getting usm or sol names vector
  names_list <- NULL
  if (!base::is.null(select)) names_list <- get_param_xml(file_path, param = select)[[1]][[select]]

  # Getting all usm or sol names from the file
  select_name <- FALSE
  if (is.null(name)) {
    # name <- names_list
  } else {
    # Checking names
    exist_names <- names_list %in% name
    if (sum(exist_names) < length(name)) {
      miss_names <- setdiff(name, names_list[exist_names])
      warning("Missing names in file: ", paste(miss_names, collapse = ","))
    }
    select_name <- TRUE
    target_name <- names_list[exist_names]
  }

  # for one name
  # param_values <- get_param_xml(file_path, param_names = param_names, select_value = select, select_value = name)[[1]]
  param_values <- get_param_xml(file_path, param = param_names)[[1]]

  # Checking if only one parameter, param_values == numerical vector
  if (length(param_names) == 1) {
    param_values <- list(param_values)
    names(param_values) <- param_names
  }

  # Getting parameters values number
  values_nb <- unlist(lapply(X = param_values, function(x) length(x)))

  # Getting expanded parameters names vector
  param <- rep(names(param_values), values_nb)

  id <- NULL
  # Calculating identifier for each occurrence of a parameter and name column
  if (base::is.null(select)) {
    id <- unlist(lapply(values_nb, function(x) {
      l <- NA
      if (x > 1) l <- 1:x
      return(l)
    }), use.names = FALSE)
    name_col <- rep(basename(file_path), sum(values_nb))
  } else {
    # Getting values nb for each usm or sol
    values_per_par <- length(names_list)

    id <- unlist(lapply(values_nb, function(x) {
      l <- rep(NA, x)
      if (x > values_per_par) l <- rep(1:(x / values_per_par), values_per_par)
      return(l)
    }),
    use.names = FALSE
    )

    name_col <- unlist(lapply(values_nb, function(x) {
      l <- names_list
      if (x > values_per_par) l <- unlist(lapply(names_list, function(y) rep(y, x / values_per_par)))
      return(l)
    }),
    use.names = FALSE
    )
  }


  # Setting file type
  type_col <- rep(get_xml_type(file_path), sum(values_nb))

  # Defining the returned data.frame
  data_df <- data.frame(
    name = name_col,
    type = type_col,
    param = param,
    id = id,
    value = unlist(param_values, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  if (select_name) {
    data_df <- dplyr::filter(data_df, name %in% target_name)
  }

  # Conversion to a wider table (with type conversion)
  if (wide_shape) {
    data_df <- df_wider(data_df)
  }


  return(data_df)
}



df_wider <- function(df, convert_type = TRUE, stringAsFactors = FALSE) {
  valid_id <- !is.na(df$id)
  df$param[valid_id] <- paste0(df$param[valid_id], "_", as.character(df$id[valid_id]))

  # parameters wider data.frame
  df <- df %>%
    dplyr::select(-"id") %>%
    tidyr::pivot_wider(names_from = "param", values_from = "value")

  if (convert_type) df <- utils::type.convert(df, as.is = !stringAsFactors)

  return(df)
}
