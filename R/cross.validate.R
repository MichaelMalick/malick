cross.validate <- function(fit, observed, data, K = 1)  {

# Function to compute prediction standard errors for general linear 
# models using leave-one-out cross validation techniques
#
# Modified code from cv.glm function in boot package
#
# data = data used to fit glm model
# fit = fitted glm object
# observed = observed y values 
# Need to specify observed column from data e.g. dat[1]
# K = number of data points to leave out
#
# Michael Malick
# 5.12.2008

    N    <- length(residuals(fit))
    pred <- data.frame(Predicted = rep(NA, N))
    obs  <- observed
    dev  <- data.frame(rep(NA,N))

        for(i in 1:N) {
            dat      <- data[-i,]
            fit.n    <- update(fit, data = dat)
            pred.n   <- predict.glm(fit.n, newdata = data[i,])
            pred[i,] <- pred.n
            dev[i,]  <- (((obs[i,] - pred[i,])^2)/N)
         }

    MSE <- sum(dev) # mean square error
    SD  <- sqrt(MSE)
    
    list(MSE = MSE, SD = SD)

}



#####################################################################
# TESTING
#####################################################################
if(FALSE) {

    set.seed(29)
    x <- rnorm(25)
    y <- rnorm(25)
    dat <- data.frame(x = x, y = y)

    fit <- glm(y ~ x) # same result as lm()
    cross.validate(fit = fit, observed = dat[1], data = dat)


}

