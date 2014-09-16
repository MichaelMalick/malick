#' @title Explore the normality assumption of a vector
#' 
#' @description
#'     Examine the distribution of data and check against normal distribution. 
#' 
#' @param x
#'     vector of numeric data
#' @param shapiro.test
#'     logical, should a Shaprio-Wilks test be run
#' @param na.rm
#'     logical, should NA values be removed before plotting
#' @param main.title
#'     main title of plot
#' 
#' @return Returns a plotting device with four diagnostic plots:
#'     \itemize{
#'         \item{histogram: includes an empirical density estimate}
#'         \item{boxplot}
#'         \item{normal qq plot}
#'         \item{empirical cdf: includes the corresponding normal
#'             cumulative density function}}
#' 
#' @author Michael Malick
#'         
#' @export
#' 
#' @seealso
#'     \code{\link{shapiro.test}}
#'     \code{\link{plot.ecdf}}
#'     \code{\link{boxplot}}
#'     \code{\link{hist}}
#'     \code{\link{density}}
#' 
#' @examples
#'     x <- rnorm(100)
#'     norm.diag(x)
#' 
norm.diag <- function(x, shapiro.test = FALSE, 
    na.rm = TRUE, main.title = "") {

    par(mfrow=c(2,2), las = 1, mar = c(4,4,5,2), bty = "o")    

    if(na.rm) x <- x[!is.na(x)]

    ## Histogram
    hist(x, main = "",
        prob = T, col = 1, bty = "o", 
        panel.first = grid(lty = 1, col = "grey90"))
        box() 
        iqd <- summary(x)[5] - summary(x)[2]
    lines(density(x, width = 2 * iqd), col = "#0080ff", lwd = 2)
    mtext("Histogram and Density Estimate", 
        font = 2, line = 1)


    ## Boxplot
    boxplot(x, main = "", col = "white", pch = 19)
    grid(lty = 1, col = "grey90")
    boxplot(x, main = "", col = "#0080ff", pch = 19, add = T,
        axes = F)
    mtext("Boxplot", font = 2, line = 1)


    ## QQ Plot
    qqnorm(x, col = "#0080ff", pch = 19, main = "",
        panel.first = grid(lty = 1, col = "grey90"))
    qqline(x, col = 1)
    mtext("Normal Q-Q Plot", font = 2, line = 1)
   

    ## CDF Plot
    plot.ecdf(x, main="", col = "#0080ff",
        panel.first = grid(lty = 1, col = "grey90"))
    LIM <- par("usr")
    y   <- seq(LIM[1], LIM[2], length=100)
    lines(y, pnorm(y, mean(x), sqrt(var(x))))
    mtext("Empirical and Normal CDF", font = 2, line = 1)


    title(main = main.title, outer = TRUE, line = -1.5, cex.main = 2)


    if(shapiro.test) shapiro.test(x)
}
