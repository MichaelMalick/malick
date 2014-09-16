#' @title Add factor means to a lattice plot
#' 
#' @description
#'     Panel function for lattice plots to add a mean bar onto an xyplot
#'     that plots a numeric ~ factor, where the the mean represents the
#'     mean of the factor.
#'     
#' @param x
#'     x values
#' @param y
#'     y values
#' @param fun
#'     \code{mean} function is used for each panel
#' @param horizontal
#'     logical
#' @param lwd
#'     line width 
#' @param lty
#'     line type
#' @param col
#'     color
#' @param col.line
#'     line color
#' @param type
#'     points or line
#' @param \dots
#'     other plotting parameters
#' 
#' @return
#'     Used as part of an \code{\link{xyplot}} call
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso
#'     \code{\link{lattice}}
#'     \code{\link{xyplot}}
#'     \code{\link{panel.abline}}
#' 
#' @examples
#'require(lattice)
#' 
#' xyplot(Sepal.Length ~ Species, data = iris,
#'     panel = function(x, y, col, ...) {
#' 	       panel.grid(h=4, v=0, col = "grey80", ...)
#' 		   panel.xyplot(x, y, col = "grey40", ...)
#' 		   panel.mean(x, y, type = "p", horizontal = F, col = 1, 
#' 		       pch = "-", cex = 10, ...) 
#'     })
#' 
panel.mean <- function (x, y, fun = mean, horizontal = TRUE, 
    lwd = reference.line$lwd, 
    lty = reference.line$lty, col, col.line = reference.line$col, 
    type = "l", ...) {

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
