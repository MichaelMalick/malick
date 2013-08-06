mm.par <- function(...) {

    # PAR SETTINGS FOR LATTICE PLOTS
    # Michael Malick
    # 09 Mar 2013
    
    usr.par <- c(...)

    fun.par <- list(
        fontsize          = list(text = 12),
        panel.background = list(col   = "grey90"),
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
        strip.background  = list(col  = c("grey80", "grey70")),
        strip.shingle     = list(col  = c("grey80", "grey70")),
        strip.border      = list(col  = "grey65"),
        axis.line         = list(col  = "grey65"),
        axis.text         = list(col  = "grey30"),
        reference.line    = list(col  = "white"),
        dot.line          = list(col  = "white"),
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





#####################################################################
# TESTING
#####################################################################
if(FALSE) {

xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
    par.settings = mm.par())

xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
    par.settings = mm.par(), grid = TRUE)

xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
    par.settings = mm.par(list(fontsize = list(text = 16),
    par.main.text = list(col = "grey50"), 
    plot.symbol = list(pch="l"))), 
    main = "Iris")

barchart(Species ~ Petal.Length, data = iris, 
    par.settings = mm.par())

barchart( ~ Petal.Length, data = iris, groups = Species,
    par.settings = mm.par(), auto.key = TRUE,
    panel = function(x, y, ...) {
        panel.grid(-1, -1)
        panel.barchart(x, y, ...)
    })

dotplot(Species ~ Petal.Length, data = iris,
    par.settings = mm.par())



}

