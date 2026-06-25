#' @title Generating a var.mod type file
#' @description Generating a daily variable list file from variables names
#' @param workspace Path of the directory containing the STICS var.mod file
#' to modify
#' @param var vector of variables names (see details)
#' @param append if TRUE, `var` data are appended to `file_name`
#' @param file_name file name to generate
#' (without path, default value: "var.mod")
#' @param stics_version   Name of the STICS version
#' (used to check variable names)
#' @param force     Force variables writing even if they are not a
#' STICS variable (default: FALSE).
#' @param verbose if TRUE displaying warning, FALSE otherwise (default)
#'
#' @details Variable names can be found using `get_var_info()`. They are
#' checked before writing. If any variable name does not exist,
#' it will not be written by default, but the function will still write
#' the variables that exist. `force= TRUE` may however be used to write
#' variables that do not exist. Variables already present in a var.mod file
#' can be listed using `get_varmod()`.
#'
#' @return None
#'
#' @examples
#' # generate a new file
#' gen_varmod(tempdir(), c("lai(n)", "hauteur"))
#' # Add a variable to the others
#' # var.mod will contain now "lai(n)","hauteur" and "masec(n)"
#' gen_varmod(tempdir(), "masec(n)", append = TRUE)
#'
#' @export
#'
gen_varmod <- function(
  workspace,
  var,
  append = FALSE,
  file_name = "var.mod",
  stics_version = "latest",
  force = FALSE,
  verbose = FALSE
) {
  # Checking if the workspace exists
  if (!dir.exists(workspace)) {
    stop(workspace, ": directory does not exist !")
  }

  file_path <- file.path(workspace, file_name)

  # Checking if file exists in append use case
  if (append && !file.exists(file_path)) {
    stop(
      file_path,
      ": file does not exist, remove append argument or set it to FALSE) !"
    )
  }

  # Just in case: unique variable names list
  var <- unique(var)

  # Checking if the variable(s) exist
  var_exist <- is_stics_var(var, stics_version, verbose = verbose)

  if (!all(var_exist) && verbose && !force) {
    message("Not any variable name to add to the var.mod file!")
  }

  if (!force) {
    var <- var[var_exist]
  }

  if (force) {
    var[var_exist] <- var_to_stics_name(var[var_exist])
  } else {
    var <- var_to_stics_name(var)
  }

  # Add possibility to append a variable to var.mod.
  if (append) {
    var <- var[!(var %in% readLines(file_path))]
  }
  # Nothing to write
  if (length(var) == 0) {
    invisible()
  }

  cat(var, file = file_path, sep = "\n", append = append)
}
