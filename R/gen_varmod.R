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
#'
#' @details Variable names can be found using `get_var_info()`. They are
#' checked before writing. If any variable name does not exist,
#' it will not be written by default, but the function will still write
#' the variables that exist. `force= TRUE` may however be used to write
#' variables that do not exist.
#'
#' @return None
#'
#' @examples
#' gen_varmod(tempdir(), c("lai(n)", "hauteur"))
#' # Add a variable to the others:
#' gen_varmod(tempdir(), "masec(n)", append = TRUE)
#' # NB: var.mod will have "lai(n)","hauteur" and "masec(n)"
#'
#' @export
#'
gen_varmod <- function(
  workspace,
  var,
  append = FALSE,
  file_name = "var.mod",
  stics_version = "latest",
  force = FALSE
) {
  # Checking if workspace exists
  if (!dir.exists(workspace)) {
    stop(paste(workspace, ": directory does not exist !"))
  }

  file_path <- file.path(workspace, file_name)

  # Checking if file exists in append use case
  if (append && isFALSE(file.exists(file_path))) {
    msg <- ": file does not exist, remove append argument or set it to FALSE) !"
    stop(paste(file_path, msg))
  }

  # Just in case: unique variable names list
  var <- unique(var)

  # Check if the variable exist:
  var_exist <- is_stics_var(var, stics_version)

  if (any(!var_exist) && isFALSE(force)) {
    var <- var[var_exist]
  }

  if (!length(var)) {
    warning("Not any variable name to add to the var.mod file!")
  }

  if (isTRUE(force)) {
    var[var_exist] <- var_to_stics_name(var[var_exist])
  } else {
    var <- var_to_stics_name(var)
  }

  # Add possibility to append a variable to var.mod.
  if (isTRUE(append)) {
    vars <- readLines(file_path)
    commonvars <- var %in% vars
    if (any(commonvars)) {
      cli::cli_alert_warning(paste0(
        "Variable{?s} {.var ",
        "{var[commonvars]}} already in",
        " {.code var.mod}. Not repeating it."
      ))
    }
    var <- var[!commonvars]
    if (length(var) == 0) {
      invisible()
    }
  }

  cat(var, file = file_path, sep = "\n", append = append)
}
