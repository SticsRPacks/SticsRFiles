#' Generates a `new_travail.usm` file from usm XML files content and some
#' switch parameters for lai forcing, parameters forcing and usm chaining.
#'
#' @param workspace Path of the directory containing the usm XML files
#' @param usm Usm name
#' @param lai_forcing 1, if `lai` is to be read from a daily lai, 0 otherwise
#' input file.
#' @param codesuite 1, if the usm is to be chained with the previous
#' simulated (for getting system state variables), 0 otherwise
#' input file.
#' @param codoptim 1, if parameters are to be read from a `param.sti` file
#' (containing parameters values to be forced), 0 otherwise
#' @param out_dir Directory path where to store the `new_travail.usm` file.
#'
#' @return None
#'
#' @keywords internal
#'
#' @noRd
#'
# @examples
gen_new_travail <- function(workspace,
                            usm,
                            lai_forcing = NULL,
                            codesuite = NULL,
                            codoptim = NULL,
                            out_dir = NULL) {

  usm_data <- get_usm_data(workspace,
                           usm,
                           lai_forcing = lai_forcing,
                           codesuite = codesuite,
                           codoptim = codoptim)

  data_plt2 <- c()
  if (usm_data$nbplantes > 1)
    data_plt2 <- c("fplt2", "ftec2", "flai2")

  data_order <- c("codesimul", "codoptim", "codesuite", "nbplantes", "nom",
                  "datedebut", "datefin", "finit", "numsol", "nomsol",
                  "fstation",
                  "fclim1", "fclim2", "nbans", "culturean", "fplt1",
                  "ftec1", "flai1", data_plt2)

  if (is.null(out_dir)) out_dir <- workspace

  out_file <- file.path(out_dir, "new_travail.usm")

  p_table <- vector(mode = "character", length = 2 * length(data_order))

  for (p in seq_along(data_order)) {
    idx <- p * 2
    p_table[idx - 1] <- paste0(":", as.character(data_order[p]))
    p_table[idx] <- usm_data[[data_order[p]]]
  }

  ret <- try(writeLines(text = p_table, con = out_file))

  if (methods::is(ret, "try-error")) {
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}


#' Get information attached to a usm from usms.xml, and possibly change
#' some forcing options
#'
#' @param workspace Path of the directory containing the usm XML files
#' @param usm Usm name
#' @param lai_forcing 1, if `lai` is to be read from a daily lai, 0 otherwise
#' input file.
#' @param codesuite 1, if the usm is to be chained with the previous
#' simulated (for getting system state variables), 0 otherwise
#' input file.
#' @param codoptim 1, if parameters are to be read from a `param.sti` file
#' (containing parameters values to be forced), 0 otherwise
#'
#' @return a named list
#'
#' @keywords internal
#'
#' @noRd
#'
#'
get_usm_data <- function(workspace,
                         usm,
                         lai_forcing = NULL,
                         codesuite = NULL,
                         codoptim = NULL) {

  data <- list()


  data <- get_param_xml(file = file.path(workspace, "usms.xml"),
                        select = "usm",
                        select_value = usm)$usms.xml

  # forcing codesimul
  # 0: culture, 1: feuille, lai forcing
  if(!is.null(lai_forcing) && lai_forcing %in% c(0,1))
    data$codesimul <- get_codesimul(lai_forcing)

  # forcing codoptim
  if(!is.null(codoptim) && codoptim %in% c(0,1))
    data$codoptim <- codoptim

  # forcing codesuite
  if(!is.null(codesuite) && codesuite %in% c(0,1))
    data$codesuite <- codesuite

  # nbplantes
  #data$nbplantes

  # nom
  data$nom <- usm

  # debut
  # data$datedebut

  # fin
  # data$datefin

  # init
  # data$finit

  # soil number
  # not used by STICS !!!!
  data$numsol <- 1

  # nomsol
  # data$nomsol

  # station
  # data$fstation

  # fclim1
  # data$fclim1

  # fclim2
  # data$fclim2

  # add constraint on culturean
  if (data$culturean != 1)
    data$culturean <- 0

  # nbans
  data$nbans <- get_years_number(
    file.path(workspace,c(data$fclim1, data$fclim2))
  )

  # culturean
  # data$culturean

  data$fplt1 <- data$fplt[1]

  data$ftec1 <- data$ftec[1]

  data$flai1 <- data$flai[1]

  if (data$flai1 == "null") data$codesimul <- get_codesimul(0)

  if (data$nbplantes > 1) {
    data$fplt2 <- data$fplt[2]

    data$ftec2 <- data$ftec[2]

    data$flai2 <- data$flai[2]
  }

  data[["ftec"]] <- NULL
  data[["fplt"]] <- NULL
  data[["flai"]] <- NULL
  data[["fobs"]] <- NULL

  return(data)
}


#' Getting the string indicating a forcing lai mode or not
#'
#' @param lai_forcing 0 for forcing mode, 0 otherwise
#'
#' @return a string, either "culture" or "feuille"

#' @keywords internal
#'
#' @noRd
#'
get_codesimul <- function(lai_forcing = 0) {

  if (lai_forcing == 0) return("culture")

  if (lai_forcing == 1) return("feuille")

  stop("Error on lai forcing value: ",
       lai_forcing,
       "\nmIt must be 0 or 1 !")
}


#' Calculating simulation years number
#'
#' @param clim_path character vector of 2 weather data files
#' for the first and the last year
#'
#' @return years number

#' @keywords internal
#'
#' @noRd
#'

get_years_number <- function(clim_path) {

  year1 <- get_year(clim_path = clim_path[1])

  if(clim_path[1] == clim_path[2]) {
    year2 <- year1
  } else {
    year2 <- get_year(clim_path = clim_path[2])
  }

  if(any(is.na(c(year1, year2))))
    stop(
      "Impossible to calculate the number of years from weather data files !"
      )

  return(year2 - year1 + 1)

}

#' Get weather data file year
#'
#' @param clim_path path of a weather data file
#'
#' @return a year (numeric)

#' @keywords internal
#'
#' @noRd
#'

get_year <- function(clim_path) {

  if(!file.exists(clim_path)) stop()

  str <- strsplit(
    trimws(readLines(con = clim_path, n = 1)),
    split = " ")[[1]][2]

  ret <- try(as.numeric(str))

  if (methods::is(ret, "try-error")) {
    return(invisible(NA))
  }

}
