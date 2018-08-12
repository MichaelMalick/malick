#' @title Add label to base graphic panels
#'
#' @description
#'  Adds a label to a base plot using the "user coordinates" of the plotting
#'  region. The default is to add the label in the upper left corner.
#'
#' @param label
#'  string, label to be added to plot
#' @param xfrac
#'  x-coordinate location, in user coordinates
#' @param yfrac
#'  y-coordinate location, in user coordinates
#' @param pos
#'  position specifier for label, see \code{\link[graphics]{text}}
#' @param ...
#'  passed to \code{graphics::text}
#'
#' @return
#'  Function only useful for side effect of adding a label.
#'
#' @author Michael Malick
#'
#' @export
#'
#' @seealso
#'  \code{\link[graphics]{text}}
#'  \code{\link[graphics]{par}}
#'
#' @examples
#'  plot(1:10)
#'  add_label("(a) cool label")
#'
add_label <- function(label, xfrac = 0.01, yfrac = 0.07, pos = 4, ...) {
    u <- graphics::par("usr")
    x <- u[1] + xfrac * (u[2] - u[1])
    y <- u[4] - yfrac * (u[4] - u[3])
    graphics::text(x, y, label, pos = pos, ...)
}
