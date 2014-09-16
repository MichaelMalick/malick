#' @title Non-parametric bootstrap of the median
#' 
#' @description 
#'     This function performs a non-parametric bootstrap of a vector with
#'     replacement. 
#' 
#' @param x a numeric vector
#' @param rep number of iterations
#' 
#' @return Returns a list:
#'     \itemize{
#'         \item{boot.mean: mean of the bootstrap samples}
#'         \item{boot.sd: standard deviation of the bootstrap samples}
#'         \item{bias: the mean of the bootstrap samples minus the
#'             median of the original data}
#'         \item{distribution: vector of the bootstrap samples}}
#' 
#' @author Michael Malick
#'
#' @export
#' 
#' @seealso \code{\link{median}} \code{\link{sample}}
#' 
#' @examples
#' x <- 1:10
#' boots <- boot.median(x)
#' 
#' hist(boots$distribution, col = "grey20")
#' abline(v = boots$boot.mean, lwd = 2, col = 2)
#' 
boot.median <- function(x, rep = 999) {
    res <- rep(NA, rep)
    for(i in 1:rep) res[i] <- median(sample(x, replace = T))
    list(boot.mean = mean(res), boot.sd = sqrt(var(res)), 
        bias = mean(res) - median(x), distribution = res)
}
