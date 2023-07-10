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
#'
#' @keywords internal
#'
#' @noRd
#'

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

  data_order <- c("codesimul", "codoptim", "codesuite", "nbplantes", "nom",
                  "datedebut", "datefin", "finit", "numsol", "nomsol",
                  "fstation",
                  "fclim1", "fclim2", "nbans", "culturean", "fplt1",
                  "ftec1", "flai1", "fplt2", "ftec2", "flai2")

  if (is.null(out_dir)) out_dir <- workspace

  out_file <- file.path(out_dir, "new_travail.usm")

  p_table <- vector(mode = "character", length = 2 * length(data_order))

  for (p in seq_along(data_order)) {
    idx <- p * 2
    p_table[idx - 1] <- paste0(":", as.character(data_order[p]))
    p_table[idx] <- usm_data[[data_order[p]]]
  }

  writeLines(text = p_table, con = out_file)

}


get_usm_data <- function(workspace,
                         usm,
                         lai_forcing = NULL,
                         codesuite = NULL,
                         codoptim = NULL) {

  data <- list()


  data <- get_param_xml(file = file.path(workspace, "usms.xml"),
                        usm, select = "usm",
                        select_value = usm)$usms.xml

  # forcing codesimul
  # 0: culture, 1: feuille, lai forcing
  if (!is.null(lai_forcing) && lai_forcing %in% c(0, 1))
    data$codesimul <- get_codesimul(lai_forcing)

  # forcing codoptim
  if (!is.null(codoptim) && codoptim %in% c(0, 1))
    data$codoptim <- codoptim

  # forcing codesuite
  if (!is.null(codesuite) && codesuite %in% c(0, 1))
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
  # in fact trhrough javaSTICS always == 1
  data$numsol <- get_numsol(soil_name = data$nomsol,
                            soil_file = file.path(workspace, "sols.xml"))

  # nomsol
  # data$nomsol

  # station
  # data$fstation

  # fclim1
  # data$fclim1

  # fclim2
  # data$fclim2

  # nbans
  data$nbans <-
    as.numeric(strsplit(x = data$fclim2, split = ".", fixed = TRUE)[[1]][2]) -
    as.numeric(strsplit(x = data$fclim1, split = ".", fixed = TRUE)[[1]][2]) +
    1

  # culturean
  # data$culturean

  data$fplt1 <- data$fplt[1]

  data$ftec1 <- data$ftec[1]

  data$flai1 <- data$flai[1]

  if (data$flai1 == "null") data$codesimul <- get_codesimul(0)

  data$fplt2 <- data$fplt[2]

  data$ftec2 <- data$ftec[2]

  data$flai2 <- data$flai[2]

  data[["ftec"]] <- NULL
  data[["fplt"]] <- NULL
  data[["flai"]] <- NULL
  data[["fobs"]] <- NULL

  data
}


get_numsol <- function(soil_name, soil_file = "sols.xml") {

  id <- which(soil_name == get_param_xml(
    file = soil_file,
    param = "sol")$sols.xml$sol)

  if (length(id > 0)) return(id)

  warning("Unknown soil name: ", soil_name)

  return(NaN)

}

get_codesimul <- function(lai_forcing = 0) {

  if (lai_forcing == 0) return("culture")

  if (lai_forcing == 1) return("feuille")

  stop("Error on lai forcing value: ",
       lai_forcing,
       "\nmIt must be 0 or 1 !")
}
