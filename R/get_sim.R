#' Load and format STICS daily output file(s)
#'
#' @description Reads and format daily output file(s) (mod_s*.sti) for usm(s)
#' with possible selection on variable names, cumulative DOY and dates
#'
#' @param workspace Vector of path(s) of directory(ies) containing the STICS
#' output files to read (mod_s*.sti file) or path of a single directory
#' containing one sub-folder per USM (named as the USM names),
#' each of them containing the corresponding STICS output file to read.
#' In the second case, the argument `usm` must also be provided.
#' @param usm Vector of USM names. Optional, if not provided, the function
#' returns the results for all USMs.
#' @param var Vector of variable names for which results have to be provided.
#' Optional, all variables considered by default. See `get_var_info()`
#' to get the list of STICS variables names.
#' @param dates list of dates to filter (POSIX date)
#' @param usms_file Path of a USM xml file. Optional, if provided, the plant
#' names are added in the Plant column (see details).
#' @param javastics Path of JavaSTICS Optional, should be provided in addition
#' to usms_file to get the plant codes if the plant files used are not
#' in the workspace but in the JavaSTICS distribution (see Details).
#' @param verbose Logical value for displaying or not information while running
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores Number of cores to use for parallel computation.
#'
#' @details
#' If `usm` is not specified (or equal to `NULL`), the
#' function reads the files from all usms in the `workspace`(s).
#'
#' If `usms_file` is provided and if the associated plant file is found,
#' the plant names in the "Plant" column of the generated `data.frame`
#' are either the plant code (as specified in the plant file) or
#' the name of the plant file, if the plant file is not found.
#'
#' If `usms_file` is not specified, the plants are named "plant_1" by default
#' (+ "plant_2" for intercrops).
#'
#' @return A list, where each element is a `data.frame` of simulation results
#' for the given usm. The list is named after the USM name.
#'
#' Intercrops are returned in a single `data.frame`, and are identified
#' using either the "Plant" or "Dominance" columns.
#'
#' See Details section for more information about the "Plant" column.
#'
#' @examples
#' path <- get_examples_path(file_type = "sti")
#' sim_data <- get_sim(path, "banana")
#'
#' @export
#'
get_sim <- function(
  workspace,
  usm = NULL,
  var = NULL,
  dates = NULL,
  usms_file = NULL,
  javastics = NULL,
  verbose = TRUE,
  parallel = FALSE,
  cores = NA
) {
  res <- get_file(
    workspace,
    usm,
    var,
    dates,
    usms_file,
    javastics,
    verbose,
    "sim",
    parallel = parallel,
    cores = cores
  )

  # Testing if results list is not empty
  # otherwise, setting "cropr_simulation" class attribute will fail
  if (length(res) > 0) {
    attr(res, "class") <- "cropr_simulation"
  }
  res
}
