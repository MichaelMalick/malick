conf.int <- function(x, alpha = 0.975, na.rm = FALSE) {

# Function to calculate confidence intervals based on the t
# distributio. na.rm is for missing values when calculating mean and
# standard deviation
#
# Michael Malick
# 12 May 2012

    avg <- mean(x, na.rm = na.rm)
    n <- length(x)
    SD <- sd(x, na.rm = na.rm)
    ci <- qt(alpha, df = n-1)*(SD/sqrt(n))

    list(Mean = avg, SD = SD, CI = ci, Upper = avg + ci, 
        Lower = avg - ci)

}



#####################################################################
# TESTING
#####################################################################
if(FALSE) {

    x <- 1:10
    conf.int(x)

    y <- c(1, 2, 3, NA, 4, 5:10)
    conf.int(y, na.rm = TRUE)

}








