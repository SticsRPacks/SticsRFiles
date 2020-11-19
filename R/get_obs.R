#' Read STICS observation files (*.obs)
#'
#' @description Read STICS observation files from a JavaStics workspace and store data into a list per usm
#'
#' @param workspace Path of a JavaStics workspace, or a vector of (recursive call).
#' @param usm_name      Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  Name of the usm file (optional)
#' @param var_list   vector of output variables names to filter (optional, see `get_var_info()` to get the names of the variables)
#' @param doy_list   vector of cumulative DOYs to filter (optional)
#' @param dates_list list of dates to filter (optional, should be a POSIX date)
#' @param javastics_path JavaStics installation path (optional, needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace). Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#'
#' @details The `.obs` files names should match USMs mames, e.g. for a usm called "banana",
#' the `.obs` file should be named `banana.obs`. For intercrops, the name should be suffixed by "p" for
#' the principal and "a" for the associated plant.
#' If `usm_name` is not specified (or equal to `NULL`), the
#' function reads the observation files from all usms listed in `usms_filename` and present in the `workspace` folder(s).
#' Or if `usms_filename` is not specified, all the .obs files are loaded from the `workspace` folder(s).
#'
#'
#' @return A named list of `data.frame`s with observations data. The list elements are named after
#' the usms names.
#'
#'
#' @examples
#' \dontrun{
#' path <- file.path(get_examples_path(file_type = "obs"),"mixed")
#'
#' # Get observations for all usms, but only banana has observations:
#' Meas <- get_obs(path)
#' # Get observations only for banana:
#' Meas_banana <- get_obs(path, "banana")
#'
#'
#' # Get oservations with real plant names when plant folder is not in the workspace:
#' get_obs(path, "banana", javastics_path= "path/to/javastics")
#' }
#'
#' @export
#'
get_obs <- function(workspace,
                    usm_name=NULL,
                    usms_filename= NULL,
                    var_list=NULL,
                    doy_list=NULL,
                    dates_list=NULL,
                    javastics_path = NULL,
                    verbose= TRUE){

  get_file(workspace,usm_name,var_list,doy_list,dates_list,usms_filename,
           javastics_path,verbose,"obs")
}

