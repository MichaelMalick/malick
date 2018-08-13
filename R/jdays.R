#' @title Compute daily time intervals for a given year
#'
#' @description
#'  Computes daily values of year, month, day of the month, week of the year,
#'  and Julian days for the input years. This function accounts for leap years.
#'  February will have 29 days in leap years.
#'
#' @param years
#'  vector of years for which to compute values
#'
#' @return
#'  Returns a data frame that contains a record for each day of the year, 365
#'  for non-leap years and 366 for leap years. If a vector of years is supplied,
#'  a "stacked" data frame of time intervals is returned.
#'
#'  For the weeks columns, the last week of the year contains an extra day (2
#'  days in leap years). For the day8 column the last period only has 5 days.
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#'  jdays(1999)
#'  jdays(1999:2001)
#'
jdays <- function(years) {

    if(!is.numeric(years)) stop("years is not numeric")

    n   <- length(years)
    lst <- vector("list", n)
    reg.yr  <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    leap.yr <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    for(i in 1:n) {
        year.i <- years[i]

        leap <- abs((year.i - 2000) %% 4) + 1 # leap == 1 is a leap year

        if(leap == 1) {
            o.year  <- rep(year.i, 366)
            o.month <- rep(1:12, leap.yr)
            o.day   <- sequence(leap.yr)
            o.jday  <- seq(1, 366, 1)
            o.day5  <- c(sort(rep(1:73, 5)), 73)
            o.day7  <- c(sort(rep(1:52, 7)), 52, 52)
            o.day8  <- c(sort(rep(1:45, 8)), rep(46, 6))
        }
        else {
            o.year  <- rep(year.i, 365)
            o.month <- rep(1:12, reg.yr)
            o.day   <- sequence(reg.yr)
            o.jday  <- seq(1, 365, 1)
            o.day7  <- c(sort(rep(1:52, 7)), 52)
            o.day5  <- c(sort(rep(1:73, 5)))
            o.day8  <- c(sort(rep(1:45, 8)), rep(46, 5))
        }
        lst[[i]] <- data.frame(year  = o.year,
                               month = o.month,
                               day   = o.day,
                               jday  = o.jday,
                               day5  = o.day5,
                               day7  = o.day7,
                               day8  = o.day8)
    }
    out <- do.call("rbind", c(lst, make.row.names = FALSE))
    return(out)
}
