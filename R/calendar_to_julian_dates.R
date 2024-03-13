##' Julian days
##'
##' Returns the Julian day equivalent to a given date.
##' @param date date you wish to convert, in the \code{\link[base]{Date}} format
##' @return integer
##' @author Timothee Flutre
get_julian_days <- function(date){
  stopifnot(methods::is(date, "Date"))
  out <- format(date, "%j")
  return(as.integer(out))
}

##' Leap year
##'
##' Returns TRUE if the given year is a leap year, FALSE otherwise
##' @param year numeric
##' @return logical
##' @author Timothee Flutre
isLeapYear <- function(year){
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
}

##' Convert dates
##'
##' Converts a calendar date into a Julian date using a start date, as used for STICS, that is, Julian day counted from January 1 of the year of the start date, typically the simulation start year.
##' Leap years are properly handled.
##' @param date date you wish to convert, in the \code{\link[base]{Date}} format
##' @param start_date date used as the reference point, in the \code{\link[base]{Date}} format, typically the simulation start year
##' @return numeric
##' @author Timothee Flutre
##' @examples
##' start_date <- as.Date("2014-08-01")
##' date <- as.Date("2015-02-10")
##' julian_date_for_stics(date=date, start_date=start_date)
##'
##' start_date <- as.Date("2008-08-01")
##' date <- as.Date("2009-02-10")
##' julian_date_for_stics(date=date, start_date=start_date)
##' @export
julian_date_for_stics <- function(date, start_date){
  stopifnot(methods::is(date, "Date"),
            methods::is(start_date, "Date"),
            date > start_date)

  out <- 0

  start_date_julian <- get_julian_days(start_date)
  start_date_year <- as.integer(format(start_date, "%Y"))

  date_julian <- get_julian_days(date)
  date_year <- as.integer(format(date, "%Y"))

  years <- seq(start_date_year, date_year)
  if(length(years) == 1){
    out <- date_julian - start_date_julian
  } else{
    years <- years[1:(length(years)-1)]
    nbLeapYears <- sum(isLeapYear(years))
    nbNonLeapYears <- sum(! isLeapYear(years))
    out <- 365 * nbNonLeapYears + 366 * nbLeapYears + date_julian
  }

  return(out)
}
