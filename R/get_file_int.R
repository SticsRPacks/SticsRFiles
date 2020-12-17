#' Read STICS observation or simulation file (*.obs or mod_s*)
#'
#' @description Read STICS observation or simulation file(s). This function is not exported because
#' it only reads and pre-formats the file without any clever tests for intercrops or usms.
#'
#' @param workspace  A directory where to read the files
#' @param filename   A vector of file names
#' @param plant_name A vector of plant names. Length of filename, or recycled if one (optional).
#' @param verbose   Logical value (optional), TRUE to display read file name, FALSE otherwise (default)
#'
#' @return A `data.frame` of all observation or simulation files concatenated by row.
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' path <- file.path(get_examples_path( file_type = "obs"))
#' get_file_int(path, "banana.obs")
#' }
get_file_int= function(workspace, filename, plant_name = NULL, verbose = TRUE){

  if (verbose) print(filename)

  if(!is.null(plant_name)){
    if(length(plant_name)>1 && length(plant_name)!=length(filename)){
      stop("length(plant_name) should be == 1 or length(filename)")
    }

    if(length(plant_name)==1){
      plant_name= rep(plant_name, length(filename))
    }
  }else{
    plant_name= filename
  }

  out_table= mapply(function(x,y){
    out = try(data.table::fread(file.path(workspace,x), data.table = FALSE))
    if(inherits(out,"try-error")){
      cli::cli_alert_warning("couldn't find valid file for {.val {file.path(workspace,x)}}")
      return(NULL)
    }
    colnames(out) = var_to_col_names(colnames(out))
    if(nrow(out) == 0) return(NULL)
    out[out<=-999.00] = NA
    out$Plant = y
    return(out)
  },x= filename, y= plant_name, SIMPLIFY= FALSE)

  out_table = dplyr::bind_rows(out_table)

  if(nrow(out_table) > 0){

    dplyr::mutate(out_table,
                  Date = as.POSIXct(x = paste(out_table$ian,out_table$mo,out_table$jo, sep="-"),
                                    format = "%Y-%m-%d",
                                    tz="UTC")) %>%
      dplyr::relocate(.data$Date) %>%
      dplyr::select(-c(.data$ian, .data$mo, .data$jo, .data$jul)) -> out_table

  }
  out_table
}

