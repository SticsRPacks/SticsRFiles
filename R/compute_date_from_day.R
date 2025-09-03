#' Convert day number into date
#'
#' Computes the date corresponding to a given day number (or vector of)
#' with reference to a start year. Typically, the start year should
#' be the year of a STICS simulation start, but not necessarily.
#' @param day day number(s) to be converted
#' @param start_year year to be used as time reference (simulation start year).
#'
#' @return Date vector
#' @author Timothee Flutre
#' @examples
#'
#' compute_date_from_day(day = 290, start_year = 1994)
#'
#' compute_date_from_day(day = 700, start_year = 1994)
#'
#' compute_date_from_day(day = 999, start_year = 1994)
#'
#' @export
#'
compute_date_from_day <- function(day, start_year) {
  stopifnot(
    all(is.numeric(day)),
    length(start_year) == 1,
    is.numeric(start_year)
  )

  out <- as.Date(day - 1, origin = paste0(start_year, "-01-01"))

  is999 <- (day == 999)
  if(any(is999)){
    idx <- which(is999)
    out[idx] <- NA
  }

  return(out)
}
