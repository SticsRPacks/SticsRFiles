##' Day of year
##'
##' Returns the day number of the year corresponding to a given date.
##' @param date date you wish to convert, in the \code{\link[base]{Date}} format
##' @return integer
##' @author Timothee Flutre
get_day_of_year <- function(date){
  stopifnot(methods::is(date, "Date"))
  out <- format(date, "%j")
  return(as.integer(out))
}

##' Leap year
##'
##' Returns TRUE if the given year is a leap year, FALSE otherwise.
##' @param year numeric
##' @return logical
##' @author Timothee Flutre
is_leap_year <- function(year){
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
}

##' Convert date into day number
##'
##' Computes the day number corresponding to a given date with reference to a start date.
##' Typically, the start date should be the date at which a STICS simulation will start.
##' Leap years are properly handled.
##' @param date date you wish to convert, in the \code{\link[base]{Date}} format
##' @param start_date date used as the reference point, in the \code{\link[base]{Date}} format, typically the simulation start year
##' @return numeric
##' @author Timothee Flutre
##' @examples
##' start_date <- as.Date("2014-08-01")
##' date <- as.Date("2015-02-10")
##' compute_day_from_date(date=date, start_date=start_date)
##'
##' start_date <- as.Date("2008-08-01")
##' date <- as.Date("2009-02-10")
##' compute_day_from_date(date=date, start_date=start_date)
##' @export
compute_day_from_date <- function(date, start_date){
  stopifnot(methods::is(date, "Date"),
            methods::is(start_date, "Date"),
            date > start_date)

  out <- 0

  start_date_julian <- get_day_of_year(start_date)
  start_date_year <- as.integer(format(start_date, "%Y"))

  date_julian <- get_day_of_year(date)
  date_year <- as.integer(format(date, "%Y"))

  years <- seq(start_date_year, date_year)
  if(length(years) == 1){
    out <- date_julian
  } else{
    years <- years[1:(length(years)-1)]
    nbLeapYears <- sum(is_leap_year(years))
    nbNonLeapYears <- sum(! is_leap_year(years))
    out <- 365 * nbNonLeapYears + 366 * nbLeapYears + date_julian
  }

  return(out)
}
