#' @title Matlab jet color palette
#' 
#' @description
#'     Replica of the `jet colors' palette in matlab.
#'     Produces a color palette using the function colorRampPalette that
#'     starts at darkblue moves through cyan, green, yellow, and ends at
#'     darkred. This color palette works well for representing mapped
#'     oceanographic data such as SST or chl-a. 
#' 
#' @param n
#'     number of levels the palette should be broken into. For
#'     example n = 100 creates 100 shades ranging over the specified
#'     colors.  
#' 
#' @return
#'     This function outputs a vector of colors with length equal to n.
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso
#'     \code{\link{colorRampPalette}} 
#'     \code{\link{heat.colors}} 
#'     \code{\link{topo.colors}} 
#'     \code{\link{terrain.colors}}
#' 
#' @examples
#' ## Create a matrix of data
#' mat <- matrix(data = seq(0.1, 100, .1), nrow = 100, ncol = 10)
#' 
#' ## Plot data using colors.ocean()
#' image(mat, col = jet.colors(100))
#' 
jet.colors <- colorRampPalette(c(
    "#00007F", 
    "blue", 
    "#007FFF", 
    "cyan",
    "#7FFF7F", 
    "yellow", 
    "#FF7F00", 
    "red", 
    "#7F0000"))
