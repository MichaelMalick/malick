#' @title Randomization test for two means
#' 
#' @description
#'     Function to test the difference between means of two samples
#'     using randomization.
#' 
#' @param x 
#'     numeric vector, sample 1
#' @param y
#'     numeric vector, sample 2
#' @param R
#'     number of iterations
#' @param one.sided
#'     logical, should the test be one-tailed
#' 
#' @return
#'     Returns a histogram of the null distribution indicating the
#'     observed difference with a red vertical line and a list with
#'     components:
#'     \itemize{
#'         \item{null.distr: null distribution}
#'         \item{data: a list giving the two vectors}
#'         \item{T: absolute value of the differences between the
#'             means of the samples}
#'         \item{p.value: the p-value of the test}
#'         \item{test: tells if its a one or two sided test}
#'     }
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso
#'     \code{\link{mean}}
#'     \code{\link{sample}}
#' 
#' @examples
#'     x <- 1:100
#'     y <- 11:110
#'     rand.mean(x, y)
#' 
rand.mean <- function(x, y, R = 1000, one.sided = FALSE) {

    T   <- abs(mean(x) - mean(y))
    n1  <- length(x)
    n2  <- length(y)
    dat <- c(x,y)
    n   <- length(dat)
    res <- rep(NA, R)

    for(i in 1:R) {
        j <- sample(1:n, n1)
        res[i] <- mean(dat[j]) - mean(dat[-j])
    }

    hist(res, nclass=20)
    abline(v = T, lwd=2, col=2)

    if(one.sided) {p <- sum(res >= T)/R; test <- "one-sided"}
    else {p <- sum(abs(res) >= T)/R; test <- "two-sided"}

    out <- list(null.distr = res, data = list(x,y), T = T, p.value =
        p, test=test) 
        
    out

}
