#' @title Check the normality assumption of a vector
#'
#' @description
#'  Check the normality assumption of univariate data or model residuals using
#'  graphical methods.
#'
#' @param x
#'  numeric vector
#' @param panel
#'  logical, if \code{TRUE} a multi-panel graphic is produced, otherwise each
#'  graphic is printed individually.
#' @param na.rm
#'  logical, should NA values be removed
#' @param title
#'  string, optional plot title placed on top outside margin
#' @param shapiro.test
#'  logical, should a Shapiro-Wilks test be run
#'
#' @return Prints a series of plots:
#'  \item{Histogram}{Histogram of data with empirical density overlay (red)
#'                   and a theoretical normal PDF overlay (grey)}
#'  \item{QQ plot}{Normal QQ plot}
#'  \item{CDF}{Empirical (red dots) and theoretical normal (grey line)
#'             cumulative distributions}
#'
#'  If \code{shapiro.test}, then the results of the test are returned
#'
#' @author Michael Malick
#'
#' @export
#'
#' @seealso
#'  \code{\link[stats]{dnorm}}
#'  \code{\link[stats]{pnorm}}
#'  \code{\link[stats]{density}}
#'  \code{\link[stats]{plot.ecdf}}
#'  \code{\link[stats]{shapiro.test}}
#'
#' @examples
#'  check_normality(rnorm(100))
#'  check_normality(rnorm(100), panel = FALSE)
#'  check_normality(rlnorm(100), title = "Title")
#'
check_normality <- function(x, panel = TRUE,
                            na.rm = FALSE,
                            title = NULL,
                            shapiro.test = FALSE) {

    if(!is.numeric(x)) stop("x is not numeric")

    if(panel) {
        .par <- graphics::par(no.readonly = TRUE)
        graphics::par(mfrow = c(2, 2), mar = c(4, 4, 5, 2))
    }

    if(na.rm) x <- x[!is.na(x)]

    ## Theoretical density
    x.extend <- grDevices::extendrange(x, f = 0.15)
    x.seq    <- seq(x.extend[1], x.extend[2], length = 100)
    x.dnorm  <- stats::dnorm(x.seq, mean(x), stats::sd(x))

    ## Emprirical density
    x.den <- stats::density(x, adjust = 1.5)

    ## Save histogram values
    x.hist <- graphics::hist(x, plot = FALSE)

    ## Compute ylim values
    ylim <- c(0, max(x.hist$density, x.den$y, x.dnorm))

    ## Histogram
    graphics::hist(x, main = "",
                   freq = FALSE,
                   ylim = ylim,
                   col = "grey70",
                   border = "white",
                   bty = "n",
                   axes = FALSE,
                   panel.first = graphics::grid(lty = 1, col = "grey90"))
    graphics::axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey50")
    graphics::axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey50")
    graphics::box(col = "grey50")
    graphics::lines(x.seq, x.dnorm, col = "grey50", lwd = 2)
    graphics::lines(x.den, col = "red3", lwd = 2)
    graphics::rug(x, col = "grey50")
    graphics::mtext("Empirical and Normal PDF", font = 2, line = 1)
    # graphics::title(sub = "Empirical = red curve, Theoretical = grey curve")
    # graphics::legend("topright", legend = c("Empirical", "Theoretical"),
    #                  lwd = 2, col = c("red3", "grey50"), bty = "n")


    ## QQ Plot
    stats::qqnorm(x, col = "red3", pch = 19, main = "",
                  cex = 0.7,
                  axes = FALSE,
                  panel.first = {graphics::grid(lty = 1, col = "grey90")
                                 stats::qqline(x, col = "grey50", lwd = 1.5)})
    graphics::axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey50")
    graphics::axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey50")
    graphics::box(col = "grey50")
    graphics::mtext("Normal Q-Q Plot", font = 2, line = 1)


    ## CDF Plot
    y <- seq(x.extend[1], x.extend[2], length = 100)
    stats::plot.ecdf(x, main = "", col = "red3", cex = 0.7,
                     axes = FALSE,
                     panel.first = {graphics::grid(lty = 1, col = "grey90")
                     graphics::lines(y, stats::pnorm(y, mean(x), stats::sd(x)),
                                     col = "grey50", lwd = 1.5)})
    graphics::axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey50")
    graphics::axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey50")
    graphics::box(col = "grey50")
    graphics::mtext("Empirical and Normal CDF", font = 2, line = 1)

    if(!is.null(title))
        graphics::title(title, outer = TRUE, line = -2)

    if(panel) graphics::par(.par)

    if(shapiro.test) stats::shapiro.test(x)
}
