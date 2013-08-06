error.bar <- function (x, y, ebl, ebu = ebl, length = 0.08, ...) {

    # Function to draw error bars on a barplot
    # x = barplot (have to name it)
    # y = vector of bar heights
    # Written By: Michael Malick
    # Last Modified: 10.22.2007

    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, 
    length = length, ...)
}



#####################################################################
# TESTING
#####################################################################
if(FALSE) {

    x <- c(10,8,13,7)
    up <- c(1,1,1,1)
    lo <- c(1,1,1,1)
     
    plt <- barplot(x, ylim = c(0, 15), las = 1)
     
    error.bar(plt, x, up, lo)
}


