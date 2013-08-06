panel.mean <- function (x, y, fun = mean, horizontal = TRUE, 
    lwd = reference.line$lwd, 
    lty = reference.line$lty, col, col.line = reference.line$col, 
    type = "l", ...) {

# Panel function for lattice plots to add a mean bar onto a plot
# that plots a numeric ~ factor, where the the mean represents
# the mean of the factor
#
# Michael Malick
# 12 May 2012



    require(lattice)

    x <- as.numeric(x)
    y <- as.numeric(y)
    reference.line = trellis.par.get("reference.line")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    if (horizontal) {
        vals <- unique(sort(y))
        yy <- seq_along(vals)
        xx <- numeric(length(yy))
        for (i in yy) xx[i] <- fun(x[y == vals[i]])
        panel.lines(xx, vals[yy], col = col.line, lty = lty, 
            lwd = lwd, type = type, ...)
    }
    else {
        vals <- unique(sort(x))
        xx <- seq_along(vals)
        yy <- numeric(length(xx))
        for (i in xx) yy[i] <- fun(y[x == vals[i]], na.rm = T)
        panel.lines(vals[xx], yy, col = col.line, lty = lty, 
            lwd = lwd, type = type, ...)
    }
}



#####################################################################
# TESTING
#####################################################################
if(FALSE) {

    require(lattice)

    xyplot(Sepal.Length ~ Species, data = iris,
        panel = function(x, y, col, ...)		{
		panel.grid(h=4, v=0, col = "grey80", ...)
		panel.xyplot(x, y, col = "grey40", ...)
		panel.mean(x, y, type = "p", horizontal = F, col = 1, 
		pch = "-", cex = 10, ...) })

}


