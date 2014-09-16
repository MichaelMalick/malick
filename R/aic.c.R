#' @title Compute AICc
#' 
#' @description 
#'     This function computes the AIC corrected for small samples sizes (AICc) and
#'     includes the additive constant. Currently only one object can be analyzed at
#'     a time.
#' 
#' @param model 
#'     linear model object produced using the \code{lm()} function
#' @param k 
#'     penalty term for additional parameters. k = 2 is standard usage.
#' 
#' @return Returns a list:
#'     \itemize{
#'         \item{edf: Effective degrees of freedom}
#'         \item{AIC: AIC value}
#'         \item{AICc: AICc value}}
#' 
#' @references 
#'     Burnham, K. P., and D. R. Anderson. 2002. Model selection and multimodel
#'     inference: a practical information-theoretical approach, 2nd edition.
#'     Springer-Verlag, New York.
#' 
#' @author Michael Malick
#' 
#' @seealso \code{\link{AIC}}
#' 
#' @export
#' 
#' @examples
#'     ## Create some data
#'     x <- rnorm(10)
#'     y <- rnorm(10)
#'     
#'     ## Fit linear model
#'     fit <- lm(y ~ x)
#'     
#'     ## Compute AICc
#'     aic.c(fit)
#' 
aic.c <- function(model, k = 2)  {
    lnL   <- logLik(model)
    P     <- attr(lnL, "df")
    N     <- length(residuals(model))
    AICx  <- -2 * c(lnL) + k * P
    AICcx <- AICx + 2 * P * (P + 1)/(N - P - 1)
    list(edf = P, AIC = AICx, AICc = AICcx)
}


