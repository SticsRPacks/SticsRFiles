#' Read STICS observation or simulation file (*.obs or mod_s*)
#'
#' @description Read STICS observation or simulation file(s). This function
#' is not exported because it only reads and pre-formats the file without any
#' clever tests for intercrops or usms.
#'
#' @param workspace  A directory where to read the files
#' @param filename   A vector of file names
#' @param plant_name A vector of plant names. Length of filename, or recycled
#' if one (optional).
#' @param verbose   Logical value (optional), TRUE to display read file name,
#' FALSE otherwise (default)
#'
#' @return A `data.frame` of all observation or simulation files concatenated
#'  by row.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom rlang .data
#' @importFrom data.table :=
#'
#' @examples
#' \dontrun{
#' path <- file.path(get_examples_path(file_type = "obs"))
#' get_file_int(path, "banana.obs")
#' }
get_file_int <- function(
  workspace,
  filename,
  plant_name = NULL,
  verbose = TRUE
) {
  # safe verbose
  if (verbose) cat(filename, "\n")

  if (is.list(filename)) filename <- unlist(filename)
  if (is.list(plant_name)) plant_name <- unlist(plant_name)

  if (!is.null(plant_name)) {
    if (length(plant_name) > 1 && length(plant_name) != length(filename)) {
      stop(
        "length(plant_name) should be == 1 or length(filename). `plant_name`:",
        plant_name,
        " filename:",
        filename
      )
    }
    if (length(plant_name) == 1) {
      plant_name <- rep(plant_name, length(filename))
    }
  } else {
    plant_name <- filename
  }

  # setting global variables for manipulating
  # data.table with := syntax
  # if not a note in checks is raised
  # "no visible binding for global variable ..."
  Plant <- Date <- ian <- mo <- jo <- .SD <- NULL

  tables <- lapply(seq_along(filename), function(i) {
    i_filename <- filename[i]
    i_plant_name <- plant_name[i]
    path <- file.path(workspace, i_filename)

    out <- tryCatch(
      {
        data.table::fread(
          path,
          data.table = TRUE,
          na.strings = c("************", "NA")
        )
      },
      error = function(e) {
        # safe warning for parallel
        warning(sprintf("couldn't find valid file for %s", path))
        return(NULL)
      }
    )

    if (is.null(out)) {
      return(NULL)
    }

    if (!"ian" %in% names(out)) {
      return(NULL)
    }

    out <- out[!is.na(out[["ian"]])]
    if (nrow(out) == 0) {
      return(NULL)
    }

    data.table::setnames(out, var_to_col_names(names(out)))
    out[out <= -999] <- NA
    out[, Plant := i_plant_name]

    out
  })

  out_table <- data.table::rbindlist(tables, use.names = TRUE, fill = TRUE)

  if (nrow(out_table) == 0) {
    warning(
      paste0(
        "Not any data loaded from Stics daily output or observation files:\n",
        paste(filename, collapse = ", ")
      )
    )
    return(out_table)
  }

  out_table[,
    Date := as.POSIXct(
      as.Date(paste(ian, mo, jo, sep = "-"), format = "%Y-%m-%d"),
      tz = "UTC"
    )
  ]

  data.table::setcolorder(
    out_table,
    c("Date", setdiff(names(out_table), "Date"))
  )

  drop_cols <- intersect(c("ian", "mo", "jo", "jul"), names(out_table))
  if (length(drop_cols) > 0) out_table[, (drop_cols) := NULL]

  out_table
}
