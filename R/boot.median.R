boot.median <- function(x, rep = 999) {

# Non-parametric bootstrap of the median
#
# Michael Malick
# 12 May 2012

    res <- rep(NA, rep)

    for(i in 1:rep) res[i] <- median(sample(x, replace = T))

    list(boot.mean = mean(res), boot.sd = sqrt(var(res)), 
        bias = mean(res) - median(x), distribution = res)

}




#####################################################################
# TESTING
#####################################################################
if(FALSE) {

    x <- 1:10
    boots <- boot.median(x)

    hist(boots$distribution, col = "grey20")
    abline(v = boots$boot.mean, lwd = 2, col = 2)
}






