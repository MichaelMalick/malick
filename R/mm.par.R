#' @title Par settings for lattice graphics
#' 
#' @description
#'     Returns a list of par settings for lattice that can be passed
#'     to the \code{par.settings} argument.    
#' 
#' @param \dots
#'     A list of parameters to pass to \code{par.settings}. This
#'     allows the user to change additional parameters not set by
#'     \code{mm.par} or to change the settings in \code{mm.par}.
#' 
#' @details
#'     A list of parameters supplied to the function should be in the
#'     same format as for \code{par.settings}.
#'     
#' @return
#'     Returns a list of visually pleasing par setting for lattice graphics.
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @seealso
#'     \code{\link{trellis.par.get}}
#'     \code{\link{trellis.par.set}}
#'     \code{\link{lattice.options}}
#'     \code{\link{xyplot}}
#' 
#' @examples
#' \dontrun{
#' require(lattice)
#' 
#' xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
#'     par.settings = mm.par())
#' 
#' xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
#'     par.settings = mm.par(), grid = TRUE)
#' 
#' xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
#'     par.settings = mm.par(list(fontsize = list(text = 16),
#'     par.main.text = list(col = "grey50"), 
#'     plot.symbol = list(pch="l"))), 
#'     main = "Iris")
#' 
#' barchart(Species ~ Petal.Length, data = iris, 
#'     par.settings = mm.par())
#' 
#' barchart( ~ Petal.Length, data = iris, groups = Species,
#'     par.settings = mm.par(), auto.key = TRUE,
#'     panel = function(x, y, ...) {
#'         panel.grid(-1, -1)
#'         panel.barchart(x, y, ...)
#'     })
#' 
#' dotplot(Species ~ Petal.Length, data = iris,
#'     par.settings = mm.par())
#' }
#' 
mm.par <- function(...) {
    
    usr.par <- c(...)

    fun.par <- list(
        fontsize          = list(text = 12),
        panel.background = list(col   = "white"),
        plot.polygon      = list(border = "transparent", 
                            col = "#0080ff"),
        # Bar colors set to same as lines and points
        superpose.polygon = list(border = "transparent", 
                            col = c("#0080ff", "#ff00ff", 
                             "darkgreen", "#ff0000", "orange", 
                             "#00ff00", "brown")),
        add.text          = list(cex = 0.8),
        plot.symbol       = list(pch  = 20),
        superpose.symbol  = list(pch  = 20),
        dot.symbol        = list(pch  = 20),
        strip.background  = list(col  = c("grey90", "grey80")),
        strip.shingle     = list(col  = c("grey90", "grey80")),
        strip.border      = list(col  = "grey45"),
        axis.line         = list(col  = "grey45"),
        axis.text         = list(col  = "grey30"),
        reference.line    = list(col  = "grey85"),
        dot.line          = list(col  = "grey85"),
        axis.components   = list(left = list(tck = 0.7),
                                 right = list(tck = 0.7),
                                 top = list(tck = 0.7),
                                 bottom = list(tck = 0.7)),
        box.dot           = list(col = "grey90", pch = "|"), 
        box.rectangle     = list(fill ="#0080ff", col = "grey30"), 
        box.umbrella      = list(col = "grey30", lty = 1))
    
    theme <- c(usr.par, fun.par)

    return(theme)
}
