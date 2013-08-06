stnd.norm <- function(x, na.rm = TRUE, tol = 0.001) {

# Standardizes a vector to N(0,1) distribution
# Michael Malick 
# 10 Jul 2013
   
    avg <- mean(x, na.rm = na.rm)
    stdev <- sd(x, na.rm = na.rm)
    stnd <- ((x - avg) / stdev)

    # if all values of the vector to be standardized are
    # equal to within the value of tol, return 0
    if(all(abs(x - avg) < tol, na.rm = na.rm))
        stnd <- rep(0, length(x))

    stnd

}


#####################################################################
# TESTING
#####################################################################
if(FALSE) {

    x <- c(1, 2, 3, NA, 4, 5)
    stnd.norm(x)

    y <- rep(3.1, 10)
    stnd.norm(y)

}




