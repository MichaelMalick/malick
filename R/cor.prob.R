#' @title Pairwise correlation matrix with p-values
#' 
#' @description
#'     The function produces a matrix of all pairwise correlations with
#'     the correlations below the main diagonal and the p-values from
#'     pairwise deletion significant tests above the main diagonal.
#' 
#' @param x
#'     Matrix or dataframe 
#' 
#' @details
#'     Correlations are performed among the columns of the matrix or dataframe.
#' 
#' @return Returns a matrix
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso \code{\link{cor}}
#' 
#' @examples
#'     x <- rnorm(25)
#'     mat <- matrix(data = x, nrow = 5, ncol = 5, dimnames = list(
#'         c("A", "B","C", "D", "E"), c("M", "N","O", "P", "Q"))) 
#' 
#'     cor.prob(mat)
#' 
#'     # Iterative version of cor Function 
#'     cor(mat, use = "pairwise.complete.obs")
#' 
cor.prob <- function(x){
    pn            <- function(x){crossprod(!is.na(x))}
    pair.SampSize <- pn(x)

    above1    <- row(pair.SampSize) < col(pair.SampSize) 
    pair.df   <- pair.SampSize[above1] - 2
    R         <- cor(x, use="pair")
    above2    <- row(R) < col(R)
    r2        <- R[above2]^2
    Fstat     <- (r2 * pair.df)/(1 - r2)
    R[above2] <- 1 - pf(Fstat, 1, pair.df)
    R
}
