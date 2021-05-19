#' Read STICS observation files (*.obs)
#'
#' @description Read STICS observation files from a JavaStics workspace and store data into a list per usm
#'
#' @param workspace Path of a JavaStics workspace with the `mod_s*.sti` files, or a vector of (recursive call).
#' @param usm_name   (optional) Vector of usms to read (optional, used to filter usms)
#' @param usms_filename  (optional) usms file path (*e.g.* "usms.xml"). Used to get the real plant name in the Plant column.
#' @param var_list   (optional) vector of output variables names to filter (see `get_var_info()` to get variables names)
#' @param dates_list (optional) list of dates to filter (POSIX date)
#' @param javastics_path (optional) JavaStics installation path Needed if the plant files are not in the `workspace`
#' but rather in the JavaStics default workspace. Only used to get the plants names.
#' @param verbose        Logical value (optional), TRUE to display infos on error, FALSE otherwise (default)
#'
#' @details The `.obs` files names must match USMs mames, *e.g.* for a usm called "banana",
#' the `.obs` file should be named `banana.obs`. For intercrops, the name should be suffixed by "p" for
#' the principal and "a" for the associated plant.
#' If `usm_name` is not specified (or equal to `NULL`), the
#' function reads the files from all usms in the `workspace`(s).
#' If `usms_filename` is not specified, the plants are named "plant_1" by default (+ "plant_2" for
#' intercrops).
#'
#' @return A list, where each element is a `data.frame`s of observations for the given usm. The list is named
#'  after the USM name. Intercrops are returned in a single `data.frame`, and are identified
#'  using either the "Plant" or "Dominance" columns.
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
                    dates_list=NULL,
                    javastics_path = NULL,
                    verbose= TRUE){

  get_file(workspace,usm_name,var_list,dates_list,usms_filename,
           javastics_path,verbose,"obs")
}

