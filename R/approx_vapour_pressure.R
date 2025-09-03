##' Approximate vapour pressure
##'
##' Approximates vapour pressure as the saturated vapour pressure at the daily mean temperature using relative humidity.
##' See section 9.3 of the STICS book, as well as M. Launay's explanations on the STICS forum (\href{https://w3.avignon.inrae.fr/forge/boards/26/topics/2237?r=2243#message-2243}{link}).
##' @param TM numeric vector of daily mean temperature
##' @param RH numeric vector of relative humidity
##' @return numeric vector
##' @author Timothee Flutre
##' @export
approx_vapour_pressure <- function(TM, RH){
  TVAR <- 6.1070 * (1 + sqrt(2) * sin(0.017453293 * TM / 3))^8.827
  return(RH * TVAR / 100)
}
