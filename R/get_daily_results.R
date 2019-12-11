#' @title Loading a daily Stics file
#' @description Reading a daily file as a tibble with possible selection
#' on variables names, cumulative DOY, dates list
#' @param workspace Stics or JavaStics workspace path
#' @param usm_name names of the usm
#' @param var_list vector of output variables names (optional)
#' @param doy_list vector of cumulative DOYs (optional)
#' @param dates_list list of dates (optional)
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file(file.path("extdata","sti","V9.0"), package = "SticsRFiles")
#' get_daily_results(path,"banana")
get_daily_results <- function(workspace,usm_name,var_list=NULL,
                              doy_list=NULL, dates_list=NULL
                              ) {
  .= NULL

  results_path=file.path(workspace,paste0("mod_s",usm_name,".sti"))

  results_tbl <- try(dplyr::as.tbl(utils::read.table(results_path,header = TRUE,
                                                     sep = ";",
                                                     stringsAsFactors = FALSE)))

  if (methods::is(results_tbl,"try-error")){
    warning("Error reading output file :",results_path)
    return()
  }

  # Add cum_jul to table (cumulative DOY)
  cum_doy <- compute_doy_cumul(results_tbl$jul, results_tbl$ian)
  results_tbl <- dplyr::mutate(results_tbl, cum_jul = cum_doy)

  # filtering dates
  # only on cum_jul
  if ( ! base::is.null(doy_list) ) {
    results_tbl <- results_tbl %>% dplyr::filter( .data$cum_jul %in% doy_list )
  }

  # selecting variables columns
  if ( ! base::is.null(var_list) ){
    col_names=make.names(var_list)
    results_tbl <- results_tbl %>% dplyr::select(c("ian", "mo","jo", "jul"),dplyr::one_of(col_names))
  }

  # Adding the Date  in the simulation results tibble
  #Date= data.frame(Date=as.POSIXct(x = paste(sim_tmp$ian,sim_tmp$mo,sim_tmp$jo, sep="-"),
  #                                 format = "%Y-%m-%d", tz="UTC"))

  results_tbl <- results_tbl %>% dplyr::mutate(Date=as.POSIXct(x = paste(.$ian,
                                                                         .$mo,
                                                                         .$jo,
                                                                         sep="-"),
                                                               format = "%Y-%m-%d",
                                                               tz="UTC")) %>%
    dplyr::select(.data$Date, dplyr::everything())



  # Using function from SticsRFiles for converting .n. to _n in the varname
  # to be homogeneous with read_obs_int outputs
  colnames(results_tbl) = var_to_col_names(colnames(results_tbl))

  # TODO
  # on ian, mo, jo
  # a %>% filter(ian==1994,mo==10,jo==17:19)
  # or on
  # Dates

  return(results_tbl)
}
