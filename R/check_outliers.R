#' @title Find potential outliers in a numeric vector
#'
#' @description
#'  Identifies potential outliers in a numeric vector based on threshold values
#'  computed from input data.
#'
#' @param x
#'  numeric vector
#' @param method
#'  method for finding potential outliers
#' @param range
#'  multiplier of IQR for outlier threshold
#' @param na.rm
#'  logical, should NA values be removed
#' @param title
#'  string, optional plot title placed on top outside margin
#' @param plot
#'  logical, should a plot of results be produced
#'
#' @return
#'  Returns a list with elements:
#'    \item{outlier}{logical vector the same length as \code{x} where
#'          \code{TRUE} indicates point is outside threshold values and
#'          \code{FALSE} otherwise}
#'    \item{index}{indices of x that are outside threshold}
#'    \item{threshold}{vector of length two giving lower and upper
#'          thresholds}
#'    \item{n.outlier}{number of points outside threshold values}
#'
#'  If \code{plot = TRUE}, then a plot of \code{x} is produced with potential
#'  outliers highlighted in red. Dashed horizontal lines indicate outlier
#'  thresholds.
#'
#' @details
#'  Currently only \code{iqr} method is implemented.
#'
#'  Method details:
#'      \itemize{
#'        \item{\code{iqr} identifies an "outlier threshold" based on the
#'                   interquartile range (IQR) of the data. The upper/lower
#'                   threshold values are calculated as +/- IQR * range.}
#'      }

#' @author Michael Malick
#'
#' @export
#'
#' @seealso
#'  \code{\link[stats]{quantile}}
#'
#' @examples
#'  check_outliers(c(rnorm(100), 10, -10))
#'  check_outliers(exp(rnorm(100)), range = 5)
#'  check_outliers(exp(rnorm(100)))
#'
check_outliers <- function(x, method = "iqr",
                           range = 3,
                           na.rm = FALSE,
                           title = NULL,
                           plot = TRUE) {

    if(!is.numeric(x)) stop("x is not numeric")

    if(na.rm) x <- x[!is.na(x)]

    if(method == "iqr") {
        qrt <- stats::quantile(x)
        q1  <- qrt[2]
        q3  <- qrt[4]
        iqr <- q3 - q1
        l.thresh <- q1 - (iqr * range)
        u.thresh <- q3 + (iqr * range)
        ind <- x < l.thresh | x > u.thresh
        out <- list(outlier = ind,
                    index = which(ind),
                    threshold = c(lower = as.vector(l.thresh),
                                  upper = as.vector(u.thresh)),
                    n.outlier = sum(ind))
        if(is.null(title)) {
            main <- paste0("Outlier: ", range, " * IQR")
        } else {
            main <- title
        }
    }

    if(plot) {
        ind <- out$outlier
        graphics::plot(x,
                       pch  = 19,
                       cex  = 0.9,
                       col  = "grey50",
                       ylab = "x",
                       xlab = "Index",
                       axes = FALSE,
                       main = main,
                       ylim = range(x, l.thresh, u.thresh),
                       panel.first = graphics::grid(lty = 1, col = "grey90"))
        graphics::abline(h = out$threshold[1], lty = 2, col = "grey40")
        graphics::abline(h = out$threshold[2], lty = 2, col = "grey40")
        graphics::points(which(ind), x[ind], pch = 19, col = "red3", cex = 1.1)
        graphics::axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey50")
        graphics::axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey50")
        graphics::rug(x[!ind], ticksize = 0.03, side = 2, col = "grey50")
        graphics::rug(x[ind], ticksize = 0.03, side = 2, col = "red3")
        graphics::box(col = "grey50")
    }
    return(out)
}
