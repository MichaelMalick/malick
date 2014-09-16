#' @title Cross validation routine for glm's
#' 
#' @description
#'     Function to compute prediction standard errors for general linear
#'     models using leave-one-out cross validation techniques
#' 
#' @param fit
#'     fitted glm object
#' @param observed 
#'     observed x values, need specify columns e.e., dat[1]
#' @param data 
#'     data used to fit glm model
#' @param k
#'     number of data points to leave out
#'     
#' @return Returns a list:
#'     \itemize{
#'         \item{MSE}{mean squared prediction error}
#'         \item{comp1 }{prediction standard deviation}}
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @details
#'     Modified code from cv.glm in boot package
#' 
#' @seealso \code{\link{glm}} \code{\link{predict}}
#' 
#' @examples
#'     set.seed(29)
#'     x <- rnorm(25)
#'     y <- rnorm(25)
#'     dat <- data.frame(x = x, y = y)
#' 
#'     fit <- glm(y ~ x) # same result as lm()
#'     cross.validate(fit = fit, observed = dat[1], data = dat)
#' 
cross.validate <- function(fit, observed, data, k = 1)  {

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
