#' Getting parameters values from a Stics text file
#'
#' @param file_path Stics text file path
#' @param param_names vector of parameters names
#' @param file_type type of file tag from "name_value", "soil", "ini", "tec"
#' @param names_dict List of correspondance between parameter names given and files intern
#' names
#' @return A vector of parameters values
#'
#@export
#'
# @examples
get_txt_param_value <- function(file_path, param_names, file_type, names_dict=NULL) {

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


