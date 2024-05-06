#' Day of year
#'
#' Returns the day number of the year corresponding to a given date.
#' @param date date you wish to convert, in the \code{\link[base]{Date}} format
#' @return integer
#' @author Timothee Flutre
#' @keywords internal
#' @noRd
get_day_of_year <- function(date){
  stopifnot(methods::is(date, "Date"))
  #out <- format(date, "%j")
  #return(as.integer(out))
  return(lubridate::yday(date))
}

#' Leap year
#'
#' Returns TRUE (or 1) if the given year is a leap year,
#' FALSE (or 0) otherwise.
#' @param year numeric
#' @param integer logical
#' @return logical or integer, either integer == FALSE (default) or TRUE
#' @author Timothee Flutre
#' @keywords internal
#' @noRd
is_leap_year <- function(year, integer = FALSE){
  is_leap <- year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
  if (integer) return(as.integer(is_leap))
}

#' Convert date into day number
#'
#' Computes the day number corresponding to a given date (or vector of)
#' from the first day of a start year. Typically, the start year should
#' be the year of a STICS simulation start.
#' Leap years are properly handled.
#' @param date date(s) vector to be converted,
#' in the character format ("YYYY-MM-DD") or \code{\link[base]{Date}} format
#' @param start_year year to be used as time reference (simulation start year).
#' Optional.
#' @return numeric vector
#' @author Timothee Flutre
#' @examples
#'
#' date <- as.Date("2015-02-10")
#' compute_day_from_date(date = date)
#'
#' compute_day_from_date(date = "2015-02-10", start_year = 2014)
#'
#' date <- as.Date("2009-02-10")
#' compute_day_from_date(date = date, start_date = 2008 )
#'
#' @export
compute_day_from_date <- function(date, start_year = NULL){
  stopifnot(methods::is(as.Date(date), "Date") || methods::is(date, "Date"),
            is.null(start_year) || methods::is(start_year, "numeric"))
            #!is.null(start_year) && lubridate::year(date) > start_year)

  # in case of several input dates
  if(length(date) > 1) {
    out <- unlist(
      lapply(date, function(x) {
        compute_day_from_date(date = x, start_year = start_year)
      }
      ))
    return(out)
  }

  out <- 0

  #start_day_of_year <- get_day_of_year(start_date)
  #start_date_year <- as.integer(format(start_date, "%Y"))
  #start_date_year <- lubridate::year(start_date)

  # Converting a character date to Date format
  if(is.character(date)) date <- as.Date(date)

  day_of_year <- get_day_of_year(date)


  #date_year <- as.integer(format(date, "%Y"))
  date_year <- lubridate::year(date)
  # No start_year, or start_year ==  date year
  # returning the current date year day
  if (is.null(start_year) || date_year == start_year) return(day_of_year)

  #years <- seq(start_date_year, date_year)
  years <- seq(start_year, date_year)
  if(length(years) == 1){
    out <- day_of_year
  } else{
    years <- years[1:(length(years)-1)]
    nbLeapYears <- sum(is_leap_year(years, integer = TRUE))
    #nbNonLeapYears <- sum(! is_leap_year(years, integer = TRUE))
    nbNonLeapYears <- length(years) - nbLeapYears
    out <- 365 * nbNonLeapYears + 366 * nbLeapYears + day_of_year
  }

  return(out)
}
