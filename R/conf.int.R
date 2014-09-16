#' @title Compute confidence intervals
#' 
#' @description
#'     Function to calculate confidence intervals (two-tailed) based on
#'     the t-distribution.
#' 
#' @param x
#'     numeric vector
#' @param alpha
#'     significance level
#' @param na.rm
#'     logical, should NA's be removed. Function fails if NA's are
#'     present and na.rm = FALSE.
#' 
#' @return Returns a list:
#'     \itemize{
#'         \item{Mean: mean of the vector}
#'         \item{SD: standard deviation of the vector}
#'         \item{CI: confidence interval}
#'         \item{Upper: upper confidence bound}
#'         \item{Lower: lower confidence bound}}
#' 
#' @author Michael Malick
#' 
#' @seealso \code{\link{gt}}
#' 
#' @export
#' 
#' @examples
#'     x <- 1:10
#'     conf.int(x)
#' 
#'     y <- c(1, 2, 3, NA, 4, 5:10)
#'     conf.int(y, na.rm = TRUE)
#' 
conf.int <- function(x, alpha = 0.975, na.rm = FALSE) {
    avg  <- mean(x, na.rm = na.rm)
    n    <- length(x)
    SD   <- sd(x, na.rm = na.rm)
    ci   <- qt(alpha, df = n-1)*(SD/sqrt(n))
    list(Mean = avg, SD = SD, CI = ci, Upper = avg + ci, 
        Lower = avg - ci)
}
