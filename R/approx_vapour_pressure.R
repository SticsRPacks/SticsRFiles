##' Approximate vapour pressure
##'
##' Approximates vapour pressure as the saturated vapour pressure at the daily mean air temperature using relative humidity.
##' See section 9.3 of the STICS book, as well as M. Launay's explanations on the STICS forum (\href{https://w3.avignon.inrae.fr/forge/boards/26/topics/2237?r=2243#message-2243}{link}).
##' @param mean_temp numeric vector of daily mean air temperature (in Celsius degrees)
##' @param rel_humid numeric vector of relative humidity (in percentage)
##' @return numeric vector
##' @author Timothee Flutre
##' @export
approx_vapour_pressure <- function(mean_temp, rel_humid) {
  stopifnot(
    length(mean_temp) == length(rel_humid),
    all(rel_humid >= 0, na.rm = TRUE),
    all(rel_humid <= 100, na.rm = TRUE)
  )

  out <- rep(NA, length(mean_temp))

  TVAR <- 6.1070 * (1 + sqrt(2) * sin(0.017453293 * mean_temp / 3))^8.827

  out <- rel_humid * TVAR / 100

  return(out)
}
