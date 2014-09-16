#' @title Linear model diagnostics
#' 
#' @description 
#'     This function produces eight standard diagnostic plots used to
#'     assess the assumptions of a linear regression. The plots analyze
#'     the residuals of the linear model. See also 
#'     \code{link{norm.diag}} for assessing the normality of the
#'     residuals.
#' 
#' @param x
#'     linear model object
#' @param main.title
#'     title for plotting device
#' 
#' @details
#'     \itemize{
#'         \item{Standardized and studentized residuals are calculated according
#'             to the functions in the MASS library}
#'         \item{Cooks distances give the influence of a point on all fitted values
#'             (4/(n-p-1) = potentially influential)}
#'     \item{Leverages give influence of a point on
#'         the model fit. Leverages over 2p/n are considered influential.
#'         ACF values +/- 2/sqrt(n) are condidered significant}}
#' 
#' @return 
#'     Returns a plotting device with eight standard diagnostic plots for linear
#'     models:
#'     \itemize{
#'         \item{histogram of the residuals}{ } 
#'         \item{normal qq plot of residuals}{ } 
#'         \item{residuals vs fitted values}{ } 
#'         \item{standardized residuals vs fitted values}{ } 
#'         \item{studentized residuals vs fitted values}{ } 
#'         \item{autocorrelation function}{ } 
#'         \item{cooks distance vs fitted}{ }
#'         \item{leverages vs fitted}{ }}
#' 
#' @references
#'     Kutner, M. H., C. J. Nachtsheim, J. Neter, and W. Li. 2005.
#'     Applied linear statistical models, 5th edition.
#'     McGraw-Hill/Irwin, New York.
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso
#'     \code{\link{lm}}
#'     \code{\link{cooks.distance}}
#'     \code{\link{stdres}}
#'     \code{\link{studres}}
#'     \code{\link{norm.diag}}
#'     \code{\link{influence}}
#' 
#' @examples
#'     ## Create some data
#'     x <- rnorm(100)
#'     y <- 1:100
#'      
#'     ## Fit linear model
#'     fitted <- lm(x ~ y)
#' 
#'     ## Run diagnostics
#'     lm.diag(fitted)
#' 
lm.diag <- function(x, main.title = "Residual Diagnostic Plots") {
    require(MASS)
    
    fit       <- x$fitted.values
    resd      <- x$residuals
    std.resd  <- stdres(x) # studentized residuals in Kutner
    stud.resd <- studres(x)
    n         <- length(x$residuals)
    p         <- length(x$coef)
    x.inf     <- influence(x)


    par(mfrow = c(2,4), oma = c(0,0,2,0), las = 1, pch = 19)

    # histogram
    hist(resd, col = "grey30", main = "Histogram of Residuals", 
        xlab = "residuals", border = "grey30",
        panel.first = grid(col = "grey85", lty = 1))
    box()


    # qq plot                          
    qqnorm(resd, col = "#0080ff",
        panel.first = grid(col = "grey85", lty = 1))
    qqline(resd, col = "#ff00ff")


    # residuals plot
    plot(fit, resd, col = "#0080ff", 
        main = "Residuals vs. Fitted Values", 
        ylab = "Residuals", xlab = "Fitted Values",
        panel.first = grid(col = "grey85", lty = 1))
    
        abline(h=0, col = "#ff00ff")


    # standardized residuals plot
    plot(fit, std.resd, 
        main = "Standardized Resd. vs Fitted Values", 
        ylab = "Standardized Residuals", xlab = "Fitted Values", 
        col = "#0080ff", panel.first = grid(col = "grey85", lty = 1))
    
        abline(h = 0, col = "#ff00ff")

    # studentized residuals plot
    plot(fit, stud.resd, 
        main = "Studentized Resd. vs Fitted Values", 
        ylab = "Studentized Residuals", xlab = "Fitted Values", 
        col = "#0080ff", panel.first = grid(col = "grey85", lty = 1))
    
        abline(h = 0, col = "#ff00ff")

    # ACF plot
    acf(resd, main = "Autocorrelation Function", col = "#0080ff",
        panel.first = grid(col = "grey85", lty = 1))

    # cooks distance plot
    plot(fit, cooks.distance(x),
        main = "Cooks Distances vs Fitted Values", 
        ylab = "Cooks Distances", xlab = "Fitted Values", 
        col = "#0080ff",
        panel.first = grid(col = "grey85", lty = 1))
        
        g <- (4/(n-p-1))
        abline (h = g, col = "#ff00ff")

    # leverages plot
    plot(fit, x.inf$hat, main = "Leverages vs Fitted Values", 
        ylab = "Leverages", xlab = "Fitted Values", col = "#0080ff",
        panel.first = grid(col = "grey85", lty = 1))
    
        v <-  ((2*p)/n)
        abline(h = v, col = "#ff00ff")

    mtext(main.title, line = 0.5, outer = TRUE, cex = 1.3)
}
