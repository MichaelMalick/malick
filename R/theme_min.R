#' @title ggplot2 Minimal Theme
#' 
#' @description 
#'     A ggolot2 theme that is simple and clean (white with grey grid lines)
#' 
#' @param base_size
#'     base text size
#' @param base_family
#'     base text family
#' @param axis_text_size
#'     size of axis tick labels
#' 
#' @return Part of a ggplot2 object
#' 
#' @author Michael Malick
#' 
#' @seealso \code{\link{ggplot} \link{theme_gray}}
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' ggplot(mpg, aes(cty, hwy)) + 
#'     geom_point(colour = "red") +
#'     theme_min()
#' 
#' ggplot(mpg, aes(cty, hwy)) + 
#'     geom_point() + 
#'     facet_wrap(~class) +
#'     theme_min()
#' 
#' ggplot(mpg, aes(cty, hwy)) + 
#'     geom_point() + 
#'     facet_grid(year~class) +
#'     theme_min()'
#' }
#'
theme_min <- function (base_size = 12, base_family = "", 
    axis_text_size = 8) {
    require(grid)
    theme_gray(base_size = base_size, base_family = base_family) %+replace%
    theme(
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA, colour = "grey50"),
        panel.grid.major = element_line(colour = "grey85", size = 0.40),
        panel.grid.major = element_line(colour = "grey90", size = 0.25),
        axis.ticks = element_line(size = 0.35, colour = "grey30"),
        axis.text = element_text(size = axis_text_size, colour = "grey30"),
        axis.title.y = element_text(vjust = 1.2, angle = 90),
        axis.title.x = element_text(vjust = -0.4),
        plot.title = element_text(vjust = 1.2),
        legend.key = element_rect(fill = NA, colour = NA),
        panel.margin = unit(0.4, units = "cm"),
        strip.background = element_rect(fill = NA, colour = NA))
}
