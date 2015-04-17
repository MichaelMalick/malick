#' @title Error bars for a barplot
#' 
#' @description
#'     Draws error bars on a barplot.
#' 
#' @param x
#'     \code{barplot} object
#' @param y
#'     vector of bar heights
#' @param ebl
#'     vector of lower error bar limits
#' @param ebu
#'     vector of upper error bar limits
#' @param length
#'     length of the horizontal bars
#' @param \dots
#'     additional parameters controlling the error bar
#' 
#' @details
#'     This function currently only works on barplots
#' 
#' @return
#'     Returns a barplot with error bars
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso \code{\link{barplot}}
#' 
#' @examples
#' ## Bar Heights
#' x <- c(10,8,13,7)
#' 
#' ## Error Bars
#' up <- c(1,1,1,1)
#' lo <- c(1,1,1,1)
#' 
#' ## Barplot
#' plt <- barplot(x, ylim = c(0, 15), las = 1)
#' error_bar(plt, x, up, lo)
#' 
error_bar <- function (x, y, ebl, ebu = ebl, length = 0.08, ...) {
    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, 
    length = length, ...)
}

