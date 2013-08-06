j.days <- function(year) {

# Returns daily values of years, months, day of the month,
# week of the year and Julian days for the input year.
# I also added indices for 5day and 8day periods.
#
# This function accounts for leap years.
# February will have 29 days in Leap Years.
#
# The last week of the year has an extra day (2 in leap years)
# For the 8day periods the last period only has 5 days
#
# Michael Malick
# 15 Mar 2013
    
    years  <- numeric()
    jdays  <- numeric()
    months <- numeric()
    weeks  <- numeric()
    days   <- numeric()
    five   <- numeric()
    eight  <- numeric()

    reg.yr  <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    leap.yr <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) 
    
    leap <- abs((year-2000) %% 4) + 1 # leap == 1 is a leap year
    
    if(leap == 1) {
        years  <- rep(year, 366) 
        jdays  <- seq(1, 366, 1)    
        weeks  <- c(sort(rep(1:52, 7)), 52, 52)
        five   <- c(sort(rep(1:73, 5)), 73)
        eight  <- c(sort(rep(1:45, 8)), rep(46, 6))
        for(i in 1:12)  { 
            months <- append(months, rep(i, leap.yr[i]))
            days   <- append(days, 1:leap.yr[i])    }
    }
    else {
        years  <- rep(year, 365) 
        jdays  <- seq(1, 365, 1)        
        weeks  <- c(sort(rep(1:52, 7)), 52)
        five   <- c(sort(rep(1:73, 5)))
        eight  <- c(sort(rep(1:45, 8)), rep(46, 5))
        for(i in 1:12)  { 
            months <- append(months, rep(i, reg.yr[i]))
            days   <- append(days, 1:reg.yr[i]) }
    }       
    data.frame(year = years, jday = jdays, month = months, 
        week = weeks, day = days, day5 = five, day8 = eight)
}





