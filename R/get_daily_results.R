#' Load and format Stics daily output file(s)
#'
#' @description Reads and format daily output file(s) (mod_s*.sti) for usm(s) with
#'  possible selection on variable names, cumulative DOY, dates list.
#'
#' @param workspace  Stics or JavaStics workspace path containing the `mod_s*.sti` files (see details)
#' @param usm_name   usm name(s) to filter
#' @param usms_filename  usms file path (e.g. "usms.xml") if any `mixed= NULL`.
#' @param var_list   vector of output variables names to filter (optional, see `get_var_info()` to get the names of the variables)
#' @param doy_list   vector of cumulative DOYs to filter (optional)
#' @param dates_list list of dates to filter (optional, should be a POSIX date)
#' @param javastics_path JavaStics installation path (Optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace)
#' @param verbose  Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#'
#' @return A list, where each element is a tibble of simulation results for the given usm. The list is named
#'  after the USM name. Intercrops are returned in a single tibble, and can be filtered
#'  using either of the "Plant" or "Dominance" columns.
#'
#' @examples
#' \dontrun{
#' path <- get_examples_path( file_type = "sti")
#' get_daily_results(path,"banana")
#' }
#' @export
#'
get_daily_results <- function(workspace,
                              usm_name=NULL,
                              var_list=NULL,
                              doy_list=NULL,
                              dates_list=NULL,
                              usms_filename= NULL,
                              javastics_path = NULL,
                              verbose= TRUE){
  res = get_file(workspace,usm_name,var_list,doy_list,dates_list,usms_filename,
                 javastics_path,verbose,"sim")

  attr(res, "class")= "cropr_simulation"
  res
}
