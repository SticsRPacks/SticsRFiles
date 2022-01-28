#' get param desc
#'
#' @param file_path csv file path
#' @param version The stics version. See `get_stics_versions_compat()` to get all compatible versions. Default
#' to "latest", a special code to get the latest version.
#' @param name a name vector for selecting loaded content using name column matching
#' @param kind a name vector for selecting loaded content using kind column matching
#'
#' @keywords internal
#'
get_param_desc <- function(file_path = NULL,
                           version = "latest",
                           name = NULL,
                           kind = FALSE) {

  # TODO
  # if file_path == dir , check if it is a JavaStics dir, calculate
  # file_path <- file.path(file_path,"config", "inputs.csv"), otherwise
  # check if the file exists in the dir
  if (base::is.null(file_path)) {
    file_path <- file.path(get_examples_path( file_type = "csv", stics_version = version),"inputs.csv")
  }



  if (!file.exists(file_path)) stop(paste("Unknown file:", file_path))

  param_df <- utils::read.csv2(file_path, header = FALSE, stringsAsFactors = F, strip.white = T)

  param_df <- param_df[,1:8]
  colnames(param_df) <- c("name", "definition", "unit", "kind", "dim", "type", "min", "max")


  if (!base::is.null(name)) {
    idx <- grep(pattern = name, x = param_df$name)
    param_df <- param_df[idx,]
  }

  par_src <- unique(param_df$kind)

  # names sorted by kind
  # if (base::is.null(name)) {
  #   par_list <- lapply(par_src, function(x) param_df[param_df$kind == x,]$name)
  #   names(par_list) <- par_src
  # }


  # Adding a version  attribute
  version <- check_version_compat(version_name = version)
  attr(x = param_df, which = "version") <- version

  return(param_df)


}
