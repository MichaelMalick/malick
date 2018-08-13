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
#'  lattice::xyplot(Sepal.Length ~ Sepal.Width, data = iris, groups = Species,
#'                  type = c("p", "smooth"),
#'                  par.settings = par_mjm())
#'
#'  lattice::xyplot(Sepal.Length ~ Sepal.Width, data = iris, groups = Species,
#'                  type = c("p", "smooth"),
#'                  par.settings = par_mjm(pch = 5:7),
#'                  auto.key = TRUE)
#'
par_mjm <- function(fontsize = 11, ...) {

    col.symbol <- "#00A0EDFF"

    col.superpose     <- c("#00A0EDFF", "#DE732DFF", "#00AB06FF", "#BE6CF5FF",
                           "#A09500FF", "#F352B1FF", "#00B3A3FF")
    col.superpose.pol <- c("#00B9FFFF", "#FA8F59FF", "#0DC649FF", "#D58CFFFF",
                           "#BBAF00FF", "#FF75CAFF", "#00CCBDFF")

    col.regions <- c("#004B9FFF", "#004AA2FF", "#0049A4FF", "#0047A6FF",
                     "#0046A8FF", "#0045A9FF", "#0043ABFF", "#0042ADFF",
                     "#0041AEFF", "#003FB0FF", "#003EB1FF", "#003CB2FF",
                     "#003AB4FF", "#0039B5FF", "#0B37B6FF", "#2936B7FF",
                     "#3934B7FF", "#4633B8FF", "#5031B9FF", "#592FBAFF",
                     "#622EBAFF", "#692DBBFF", "#702BBBFF", "#772ABBFF",
                     "#7D29BCFF", "#8328BCFF", "#8827BCFF", "#8D26BCFF",
                     "#9325BCFF", "#9725BCFF", "#9C25BCFF", "#A125BBFF",
                     "#A525BBFF", "#A925BBFF", "#AD26BAFF", "#B127BAFF",
                     "#B528B9FF", "#B929B9FF", "#BC2BB8FF", "#C02DB7FF",
                     "#C32EB6FF", "#C730B5FF", "#CA32B4FF", "#CD35B3FF",
                     "#D037B2FF", "#D339B1FF", "#D63CB0FF", "#D93EAFFF",
                     "#DB41ADFF", "#DE44ACFF", "#E146ABFF", "#E349A9FF",
                     "#E54CA8FF", "#E84FA6FF", "#EA52A4FF", "#EC54A3FF",
                     "#EE57A1FF", "#F05A9FFF", "#F25D9DFF", "#F4609BFF",
                     "#F66399FF", "#F86697FF", "#FA6994FF", "#FB6D92FF",
                     "#FD7090FF", "#FE738DFF", "#FF768BFF", "#FF7988FF",
                     "#FF7C86FF", "#FF7F83FF", "#FF8280FF", "#FF867EFF",
                     "#FF897BFF", "#FF8C78FF", "#FF8F75FF", "#FF9271FF",
                     "#FF966EFF", "#FF996BFF", "#FF9C68FF", "#FF9F64FF",
                     "#FFA261FF", "#FFA65DFF", "#FFA959FF", "#FFAC56FF",
                     "#FFAF52FF", "#FFB34EFF", "#FFB64AFF", "#FFB946FF",
                     "#FFBC42FF", "#FFC03EFF", "#FFC339FF", "#FFC635FF",
                     "#FFC931FF", "#FFCD2DFF", "#FFD029FF", "#FFD325FF",
                     "#FFD722FF", "#FFDA1FFF", "#FFDD1CFF", "#FFE01BFF")
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
