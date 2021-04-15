#' Load and format Stics daily output file(s)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because we realised that it's
#' a special case of the [get_sim()]
#'
#' @examples
#'  \dontrun{
#' get_daily_results(path,"banana")
#' # ->
#' get_sim(path,"banana")
#' }
#'
#' @keywords internal
#'
#' @export


get_daily_results <- function(...) {

  lifecycle::deprecate_warn(
    "0.3.0",
    "get_daily_results()",
    "get_sim()",
    details = "This function is a special case of get_sim(); use it instead.")
  get_sim(...)
}

#' Load and format Stics daily output file(s)
#'
#' @description Reads and format daily output file(s) (mod_s*.sti) for usm(s) with
#'  possible selection on variable names, cumulative DOY and dates
#'
#' @param workspace Path of a JavaStics workspace with the `mod_s*.sti` files, or a vector of (recursive call).
#' @param usm_name      Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  (optional) usms file path (*e.g.* "usms.xml"). Used to get the real plant name in the Plant column.
#' @param var_list   (optional) vector of output variables names to filter (see `get_var_info()` to get variables names)
#' @param doy_list   (optional) vector of cumulative DOYs to filter (integers)
#' @param dates_list (optional) list of dates to filter (POSIX date)
#' @param javastics_path (optional) JavaStics installation path Needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace. Only used to get the plants names.
#' @param verbose    (optional) Logical value, `TRUE` to display infos on error, `FALSE` otherwise (default)
#'
#' @details If `usm_name` is not specified (or equal to `NULL`), the
#' function reads the files from all usms in the `workspace`(s).
#' If `usms_filename` is not specified, the plants are named "plant_1" by default (+ "plant_2" for
#' intercrops).
#'
#' @return A list, where each element is a `data.frame`s of simulation results for the given usm. The list is named
#'  after the USM name. Intercrops are returned in a single `data.frame`, and are identified
#'  using either the "Plant" or "Dominance" columns.
#'
#' @examples
#' \dontrun{
#' path <- get_examples_path(file_type = "sti")
#' get_sim(path,"banana")
#' }
#' @export
#'
get_sim <- function(workspace,
                    usm_name=NULL,
                    var_list=NULL,
                    doy_list=NULL,
                    dates_list=NULL,
                    usms_filename= NULL,
                    javastics_path = NULL,
                    verbose= TRUE){
  res = get_file(workspace,usm_name,var_list,doy_list,dates_list,usms_filename,
                 javastics_path,verbose,"sim")

  # Testing if results list is not empty
  # otherwise, setting "cropr_simulation" class attribute will fail
  if (length(res)) attr(res, "class") = "cropr_simulation"

  res
}
