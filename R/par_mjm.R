#' @title Minimalist lattice theme
#'
#' @description
#'  A minimalist theme for lattice graphics
#'
#' @param fontsize
#'  font size for labels
#' @param ...
#'  passed to \code{par.settings}
#'
#' @details
#'  see \code{latticeExtra::custom.theme} for other examples
#'
#' @export
#'
#' @seealso
#'  \code{\link{trellis.par.get}}
#'
#' @author Michael Malick
#'
#' @examples
#' lattice::show.settings(par_mjm())
#'
#'  lattice::xyplot(Sepal.Length ~ Sepal.Width, data = iris, groups = Species,
#'                  type = c("p", "smooth"),
#'                  par.settings = par_mjm())
#'
#'  lattice::xyplot(Sepal.Length ~ Sepal.Width, data = iris, groups = Species,
#'                  type = c("p", "smooth"),
#'                  par.settings = par_mjm(pch = 5:7),
#'                  auto.key = TRUE)
#'
#'  lattice::bwplot(Species ~ Sepal.Width, data = iris,
#'                  par.settings = par_mjm())
#'
#'  lattice::levelplot(volcano, par.settings = par_mjm())
#'  lattice::xyplot(rnorm(100) ~ rnorm(100), par.settings = par_mjm())
#'
par_mjm <- function(fontsize = 11, ...) {

    ## RColorBrewer::brewer.pal(8, "Dark2")[1]
    col.symbol <- "#1B9E77"

    ## RColorBrewer::brewer.pal(8, "Dark2")
    col.superpose <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                       "#E6AB02", "#A6761D", "#666666")

    ## RColorBrewer::brewer.pal(8, "Pastel2")
    col.superpose.pol <- c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4",
                           "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")

    ## viridis::plasma(100)
    col.regions <- c("#0D0887FF", "#150789FF", "#1B068DFF", "#220690FF",
                     "#270591FF", "#2C0594FF", "#300597FF", "#350498FF",
                     "#39049AFF", "#3E049CFF", "#43039EFF", "#47039FFF",
                     "#4B03A1FF", "#4F02A2FF", "#5302A3FF", "#5701A4FF",
                     "#5B01A5FF", "#6001A6FF", "#6300A7FF", "#6700A8FF",
                     "#6B00A8FF", "#6F00A8FF", "#7301A8FF", "#7701A8FF",
                     "#7B02A8FF", "#7F03A8FF", "#8305A7FF", "#8707A6FF",
                     "#8A09A5FF", "#8E0CA4FF", "#910EA3FF", "#9511A1FF",
                     "#99149FFF", "#9C179EFF", "#9F1A9DFF", "#A21D9AFF",
                     "#A62098FF", "#A92296FF", "#AC2694FF", "#AF2892FF",
                     "#B22B8FFF", "#B52F8CFF", "#B7318AFF", "#BB3488FF",
                     "#BD3786FF", "#C03A83FF", "#C23D81FF", "#C5407EFF",
                     "#C8437BFF", "#CA457AFF", "#CC4977FF", "#CE4B75FF",
                     "#D14E72FF", "#D35271FF", "#D5546EFF", "#D8576BFF",
                     "#DA5A6AFF", "#DC5D67FF", "#DE6065FF", "#E06363FF",
                     "#E26560FF", "#E4695EFF", "#E66C5CFF", "#E76F5AFF",
                     "#E97257FF", "#EB7556FF", "#ED7953FF", "#EF7B51FF",
                     "#F07F4FFF", "#F1824CFF", "#F3864BFF", "#F48948FF",
                     "#F58C46FF", "#F79044FF", "#F89441FF", "#F9973FFF",
                     "#FA9B3DFF", "#FA9E3BFF", "#FBA238FF", "#FCA536FF",
                     "#FCA934FF", "#FDAD32FF", "#FDB130FF", "#FDB52EFF",
                     "#FEB92CFF", "#FEBD2AFF", "#FDC129FF", "#FDC527FF",
                     "#FDC926FF", "#FCCD25FF", "#FCD225FF", "#FBD624FF",
                     "#FADA24FF", "#F8DE25FF", "#F7E225FF", "#F6E726FF",
                     "#F5EC27FF", "#F3F027FF", "#F1F426FF", "#F0F921FF")

    lwd <- 0.8

    theme <- list(
        fontsize          = list(text = fontsize),
        par.main.text     = list(font = 1),
        strip.background  = list(col  = c("grey93", "grey93")),
        strip.shingle     = list(col  = c("grey65", "grey65")),
        strip.border      = list(col  = "grey50", lwd = rep(lwd, 7)),
        axis.components   = list(right  = list(tck = 0.5),
                                 top    = list(tck = 0.5),
                                 left   = list(tck = 0.5),
                                 bottom = list(tck = 0.5)),
        axis.line         = list(col = "grey50", lwd = lwd),
        plot.symbol       = list(pch = 16, col = col.symbol),
        plot.line         = list(pch = 16, col = "grey10"),
        add.line          = list(col = "grey10"),
        superpose.symbol  = list(col = col.superpose, pch = 16),
        superpose.line    = list(col = col.superpose),
        regions           = list(col = col.regions),
        dot.symbol        = list(col = col.symbol),
        plot.polygon      = list(col = "grey50", border = "white"),
        superpose.polygon = list(col = col.superpose.pol, border = "white"),
        box.rectangle     = list(col = "grey40"),
        box.dot           = list(col = "grey10"),
        box.umbrella      = list(col = "grey40", lty = 1),
        box.3d            = list(col = "grey70", lwd = lwd))
    utils::modifyList(utils::modifyList(lattice::standard.theme("pdf"), theme),
               lattice::simpleTheme(...))
}
