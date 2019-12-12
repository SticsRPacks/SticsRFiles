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

get_name_value_file_value <- function(file_path, param_names, names_dict=NULL) {
  # Case plt, station, tempopar.sti, tempoparv6.sti

  # browser()

  lines_list <- readLines(file_path, warn = FALSE)


  # removing leading/trailing blanks and multiple blanks
  lines_list <- strsplit(gsub("\\s+", " ", trimws(lines_list)), " ")


  couples_nb <- length(lines_list)/2

  sel <- rep(c(TRUE,FALSE), couples_nb)

  par_list <- lines_list[ sel ]
  par_val <- lines_list[! sel ]


  par_idx <- par_list %in% param_names

  # TODO : A recuperer pour traiter le sol
  # par_val <- lapply(par_val[par_idx],function(x) unlist(strsplit(x, " ")))


  #browser()

  par_idx <- !is.na(lapply(par_val, as.numeric))
  num_val <- lapply( par_val[par_idx], function(x) x <- as.numeric(x))
  par_val[par_idx] <- num_val



  return(par_val)


}
