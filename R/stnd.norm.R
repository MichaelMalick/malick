#' @title Standardizes a vector to a N(0, 1) distribution
#' 
#' @description
#'     This function standardizes a vector by taking each element of the
#'     vector and subtracting the mean and dividing by the standard
#'     deviation of the vector.
#' 
#' @param x
#' 	numeric vector
#' @param na.rm
#'     logical, if TRUE NA's are removed for computing the mean and
#'     standard deviation.
#' @param tol
#'     tolerance of similarity between elements in the vector, if
#'     each element is within the tolerance of each other a vector
#'     of zeros is returned
#' 
#' @return
#'     Returns a vector the same length as x
#' 
#' @details
#'     You can set a tolerance level, which is required if the values in
#'     the vector to be standardized are equal. If all values in the
#'     vector are within the 'tol' level of each other the standardized
#'     values will be set to 0.
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso
#'     \code{\link{mean}}
#'     \code{\link{sd}}
#' 
#' @examples
#' x <- c(1, 2, 3, NA, 4, 5)
#' stnd.norm(x)
#' 
#' y <- rep(3.1, 10)
#' stnd.norm(y)
#' 
stnd.norm <- function(x, na.rm = TRUE, tol = 0.001) {

   
    avg <- mean(x, na.rm = na.rm)
    stdev <- sd(x, na.rm = na.rm)
    stnd <- ((x - avg) / stdev)

    # if all values of the vector to be standardized are
    # equal to within the value of tol, return 0
    if(all(abs(x - avg) < tol, na.rm = na.rm))
        stnd <- rep(0, length(x))

    stnd

}
