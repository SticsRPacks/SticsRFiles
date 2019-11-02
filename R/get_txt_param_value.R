#' @title Getting parameters values from Stics text files
#'
#' @description Extracting parameter value from a text input file for a given list
#' of parameters names or all values
#'
#' @param file_path file path
#' @param param_names character vector of parameters names
#' @param file_type key word designing a file structure type (optional)
#' @param names_dict A list containining for a given field name the corresponding
#' Stics parameter name (optional)
#'
#' @return a named list of parameters values
#'
#' @export

get_txt_param_value <- function(file_path, param_names = NULL,
                                file_type = "name_value",
                                names_dict=NULL) {


  if ( ! is.null(names_dict) ) {
    warning("The use of the names dictionnary is not implemented yet !")
  }

  files_types <- c("name_value", "soil", "ini", "tec")

  par_val <- list()

  if ( ! is.element(file_type, files_types) ) stop("Unknown file type !")

  if ( ! file.exists(file_path)) {
    stop("Unknown file !")
  }

  not_implemented <- paste(file_type, "is not taken into account yet !")

  switch(file_type,
         name_value = { par_val <- get_name_value_file_value(file_path,param_names) },
         soil = { warning(not_implemented) },
         ini = { warning(not_implemented) },
         tec = { warning(not_implemented) }
  )


  return(par_val)

}

get_name_value_file_value <- function(file_path, param_names = NULL, names_dict=NULL) {
  # Treating cases: plt, station, tempopar.sti, tempoparv6.sti

  lines_list <- readLines(file_path, warn = FALSE)


  # removing leading/trailing blanks and multiple blanks
  lines_list <- strsplit(gsub("\\s+", " ", trimws(lines_list)), " ")


  couples_nb <- length(lines_list)/2

  sel <- rep(c(TRUE,FALSE), couples_nb)

  # splitting parameters names and parameters values
  # with removing names end matching (n)
  par_list <- lapply(lines_list[ sel ], function(x) gsub("\\([0-9]\\)$","",x))
  par_list <- unlist(par_list)
  par_val <- lines_list[! sel ]

  unique_names <- unique(par_list)

  # getting all parameter names if no param_names given as arg
  if (is.null(param_names)) {
    param_names <- unique_names
  } else {
    # filtering unexisting parameters names
    existing_idx <- unique_names %in% param_names
    if ( sum(existing_idx) != length(param_names) ) {
      warning("At least one parameter name does not exist in file!")
      param_names <- unique_names[existing_idx]
    }
  }

  if (! length(param_names)) {
    warning("Not any parameter exists in file!")
    return()
  }



  # Getting parameters names indices in par_list for each param_names
  par_idx <- lapply(param_names, function(x) par_list %in% x)
  # Getting parameters values list according to parameters names
  par_values <- lapply(par_idx, function(x) as.numeric(par_val[x]))
  names(par_values) <- param_names

  # Fixing values that are not numeric, if any
  # Getting non numeric parameters names and idx in the original
  # parameters values list (par_list)
  non_numeric_names <- param_names[unlist(lapply(par_values, function(x) all(is.na(x))))]

  # Not any non numeric parameters, exiting
  if (! length(non_numeric_names)) return(par_values)

  non_numeric_idx <- lapply(non_numeric_names, function(x) par_list %in% x)

  # Getting non numeric parameters names indexes in the final parameters names
  non_num_par_idx <- which(param_names %in% non_numeric_names)
  # Setting non numeric parameters values with strings
  for (i in 1:length(non_num_par_idx)) {
    par_values[[non_num_par_idx[i]]] <- unlist(par_val[non_numeric_idx[[i]]])
  }



  # TODO : A recuperer pour traiter le sol
  # par_val <- lapply(par_val[par_idx],function(x) unlist(strsplit(x, " ")))




  return(par_values)


}
