cor.prob <- function(X){

# Pairwise Correlation Significant Tests
# Correlations Below Main Diagonal
# Significance Tests with Pairwise Deletion Above Main Diagonal
# Obtained online
# Modified by Michael Malick
# 12 May 2012

    pn            <- function(X){crossprod(!is.na(X))}
    pair.SampSize <- pn(X)

    above1    <- row(pair.SampSize) < col(pair.SampSize) 
    pair.df   <- pair.SampSize[above1] - 2
    R         <- cor(X, use="pair")
    above2    <- row(R) < col(R)
    r2        <- R[above2]^2
    Fstat     <- (r2 * pair.df)/(1 - r2)
    R[above2] <- 1 - pf(Fstat, 1, pair.df)
    R
}


#####################################################################
# TESTING
#####################################################################
if(FALSE) {
    
    x <- rnorm(25)
    mat <- matrix(data = x, nrow = 5, ncol = 5, dimnames = list(
        c("A", "B","C", "D", "E"), c("M", "N","O", "P", "Q"))) 

    cor.prob(mat)

    # Iterative Version of cor Function 
    cor(mat, use = "pairwise.complete.obs")

}


