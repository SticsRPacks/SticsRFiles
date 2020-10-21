#' Read STICS observation file (*.obs)
#'
#' @description Read STICS observation file(s). This function is not exported because
#' it only read and pre-formate an obs file without any clever tests for intercrops or usms.
#'
#' @param workspace  A directory where to read the files
#' @param filename   A vector of observation file names
#' @param plant_name A vector of plant names. Length of filename, or recycled if one (optional).
#' @param verbose   Logical value (optional), TRUE to display read file name, FALSE otherwise (default)
#'
#' @return A `data.frame` of all observation files concatenated by row.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' path <- file.path(get_examples_path( file_type = "obs"))
#' get_obs_int(path, "banana.obs")
#' }
get_obs_int= function(workspace, filename, plant_name = NULL, verbose = TRUE){

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

  obs_table= mapply(function(x,y){
    out= data.table::fread(file.path(workspace,x), data.table = F)
    out[out<=-999.00]= NA
    colnames(out)= var_to_col_names(colnames(out))
    if(nrow(out) == 0) return(NULL)
    out$Plant= y
    return(out)
  },x= filename, y= plant_name, SIMPLIFY= FALSE)

  obs_table = dplyr::bind_rows(obs_table)

  if(nrow(obs_table) > 0){
    # Adding the date column:
    obs_table = dplyr::transmute(obs_table,Date = as.POSIXct(x = paste(.data$ian,.data$mo,
                                                             .data$jo, sep="-"),
                                                   format = "%Y-%m-%d", tz="UTC"),
                                 dplyr::across(dplyr::everything()))
  }

  obs_table
}

