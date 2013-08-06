aic.c <- function(model, k = 2)  {

# Function to calculate AIC and AICc
# Does include additive constant
#
# Michael Malick
# January 23, 2008
        
    lnL   <- logLik(model)
    P     <- attr(lnL, "df")
    N     <- length(residuals(model))
    AICx  <- -2 * c(lnL) + k * P
    AICcx <- AICx + 2 * P * (P + 1)/(N - P - 1)
    list(edf = P, AIC = AICx, AICc = AICcx)
}



#####################################################################
# TESTING
#####################################################################
if(FALSE) {
    set.seed(29)
    x <- rnorm(100)
    y <- rnorm (100)
    fit <- lm(y ~ x)
    fit.null <- lm(y ~ 1)


    aic.c(fit)
    aic.c(fit.null)
}

