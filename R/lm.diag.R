lm.diag <- function(x, main.title = "Residual Diagnostic Plots") {

# Linear model residual analysis and outlier detection
#
# Gives plots of: 
#   histogram of residuals 
#   qq plot of residuals 
#   residuals vs. fitted 
#   standardized residuals vs fitted 
#   studentized residuals vs fitted 
#   cooks distance vs fitted 
#   leverages vs fitted
#
# Standardized and Studentized are calculated according to MASS
#
# Cooks distances give the influence of a point on all fitted values
#   4/(n-p-1) = poetentially influential 
#
# Leverages give influence of a point on the model fit
#   Leverages over 2p/n are considered influencial
#
# Michael Malick
# 10 Jul 2013

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



#####################################################################
# TESTING
#####################################################################
if(FALSE) {
    ## Create some data
    x <- rnorm (25)
    y <- seq(1, 25, 1)
     
    ## Fit linear model
    fit <- lm(x ~ y)

    ## Run diagnostics
    lm.diag(fit)
} 




