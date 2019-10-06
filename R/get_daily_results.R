get_daily_results <- function(workspace,usm_name,var_list=NULL,
                              doy_list=NULL, dates_list=NULL) {
  #' @title Loading a daily Stics file
  #' @description Reading a daily file as a tibble with possible selection
  #' on variables names, cumulative DOY, dates list
  #' @param workspace Stics or JavaStics workspace path
  #' @param usm_name names of the usm
  #' @param var_list vector of output variables names (optional)
  #' @param doy_list vector of cumulative DOYs (optional)
  #' @param dates_list list of dates (optional)
  #' @export
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2019-07-09 16:40:48 +0200 (mar. 09 juil. 2019) $
  #  $Author: sbuis $
  #  $Revision: 1491 $
  # ----------------------------------------------------------------------


  results_path=file.path(workspace,paste0("mod_s",usm_name,".sti"))

  results_tbl <- try(dplyr::as.tbl(utils::read.table(results_path,header = TRUE,
                                                     sep = ";",
                                                     stringsAsFactors = FALSE)))

  if (methods::is(results_tbl,"try-error")){
    stop("Error reading output file :",results_path)
  }

  # Add cum_jul to table (cumulative DOY)
  cum_doy <- compute_doy_cumul(results_tbl$jul, results_tbl$ian)
  results_tbl <- dplyr::mutate(results_tbl, cum_jul = cum_doy)

  # filtering dates
  # only on cum_jul
  if ( ! is.null(doy_list) ) {
    results_tbl <- results_tbl %>% dplyr::filter( cum_jul %in% doy_list )
  }

  # selecting variables columns
  if ( ! is.null(var_list) ){
    col_names=make.names(var_list)
    results_tbl <- results_tbl %>% dplyr::select(c("ian", "mo","jo", "jul"),dplyr::one_of(col_names))
  }

  # TODO
  # on ian, mo, jo
  # a %>% filter(ian==1994,mo==10,jo==17:19)
  # or on
  # Dates (to be added to the table)
  
  # Using function from SticsRFiles for converting .n. by _n in the varname
  # to be homogeneous with read_obs outputs
  results_tbl = Del_spe_col(results_tbl)



  return(results_tbl)
}
