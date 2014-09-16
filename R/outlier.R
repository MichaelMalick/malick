#' @title Identify outliers in a series of residuals
#' 
#' @description
#'     Identifies outliers in a series of residuals according their index number in
#'     the vector.
#' 
#' @param x
#'     vector of residuals
#' @param type
#'     One of \code{"outlier"}, \code{"max.abs"}, or \code{"max.min"}. 
#' @param p
#'     p-value
#' @param plot.it
#'     logical, should the results be plotted
#' 
#' @return Returns a list with two components:
#'     \itemize{ 
#'         \item{positive}{positive outliers}
#'         \item{negative}{negative outliers}}
#' 
#'     There are three types:
#'     \itemize{
#'         \item{outlier}{
#'             returns the positions of all residuals that are 
#'             larger than (smaller than) the (1-p/2)th (p/2th) percentile 
#'             of the distribution of the residuals assuming they follow a 
#'             normal distribution.
#'         }
#' 
#'         \item{max.abs}{
#'             returns the position and sign of the maximum absolute
#'             residual
#'         }
#' 
#'         \item{max.min}{
#'             returns the positions of the maximum and minimum residual 
#'         }
#'     }
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso \code{\link{qqnorm}}
#' 
#' @examples
#' set.seed(29)
#' x <- rnorm(25)
#' y <- rnorm(25)
#' fit <- lm(y ~ x)
#' outlier(resid(fit))
#' 
outlier <- function(x, type = "outlier", p = 0.05, plot.it = TRUE) {

    if(type == "max.abs") {
        pos <- seq(along = x)
        j <- !is.na(x)
        pos <- pos[j]
        x1 <- x[j]
        out <- pos[abs(x1) == max(abs(x1))]
        if(length(out) > 1)
            warning("More than one outlier with the same value")
        Sign <- sign(x[out])
        res <- list(index = out, sign = Sign)
    }
    if(type == "max.min") {
        pos <- seq(along = x)
        j <- !is.na(x)
        pos <- pos[j]
        x1 <- x[j]
        Max <- pos[x1 == max(x1)]
        Min <- pos[x1 == min(x1)]
        if(length(Max) > 1.)
            warning("More than one maximum with the same value")
        if(length(Min) > 1.)
            warning("More than one minimum with the same value")
        res <- list(maximum = Max, minimum = Min)
    }
    if(type == "outlier") {
        pos <- seq(along = x)
        j <- !is.na(x)
        pos <- pos[j]
        x1 <- x[j]
        sd <- sqrt(var(x1))
        out.pos <- pos[x1 >= qnorm(1 - p/2, mean(x1), sd)]
        out.neg <- pos[x1 <= qnorm(p/2, mean(x1), sd)]
        if(plot.it) {
            plot(x)
            abline(h = 0)
            abline(h = qnorm(c(p/2, 1 - p/2), mean(x1), sd), lty = 2)
        }
        res <- list(positive = out.pos, negative = out.neg)
    }
    res
}          
