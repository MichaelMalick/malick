rand.mean <- function(x, y, R = 1000, one.sided = FALSE) {

# Function to test difference between means of two samples using
# randomization

    T   <- abs(mean(x) - mean(y))
    n1  <- length(x)
    n2  <- length(y)
    dat <- c(x,y)
    n   <- length(dat)
    res <- rep(NA, R)

    for(i in 1:R) {
        j <- sample(1:n, n1)
        res[i] <- mean(dat[j]) - mean(dat[-j])
    }

    hist(res, nclass=20)
    abline(v = T, lwd=2, col=2)

    if(one.sided) {p <- sum(res >= T)/R; test <- "one-sided"}
    else {p <- sum(abs(res) >= T)/R; test <- "two-sided"}

    out <- list(null.distr = res, data = list(x,y), T = T, p.value =
        p, test=test) 
        
    out

}


#####################################################################
# TESTING
#####################################################################
if(FALSE) {

    x <- 1:100
    y <- 11:110
    rand.mean(x, y)

}

