
#' Tableau fill scales (gradient)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param guide Type of legend. Use 'colourbar' for continuous colour bar, or 'legend' for discrete colour legend.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{continuous_scale}}.
#' @export
#'
#' @examples library(ggplot2)
#' library(paltableau)
#' ggplot(iris)+
#'   geom_col(aes(Sepal.Length, Sepal.Width, fill = Sepal.Length))+
#'   scale_fill_tableau_gradient(palette = pal_tableau$diverging$temperature,
#'                               direction = 1,
#'                               start = 0.2,end = 0.8)

scale_fill_tableau_gradient = function(palette = pal_tableau$sequential$blue_teal,
                                       direction = 1, na.value = "grey50",
                                       start = 0, end = 1,
                                       guide = "colourbar",  ...) {
  manual_rescale = rescale_gradient(palette,direction,start,end)
  ggplot2::continuous_scale("fill",
                            palette = manual_rescale,
                            na.value = na.value,
                            guide = guide, ...)
}


#' Tableau color scales (gradient)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param guide Type of legend. Use 'colourbar' for continuous colour bar, or 'legend' for discrete colour legend.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{continuous_scale}}.
#' @export
#' @examples library(ggplot2)
#' library(paltableau)
#' ggplot(iris)+
#'   geom_point(aes(Sepal.Length, Sepal.Width, color = Sepal.Width))+
#'   scale_color_tableau_gradient(palette = pal_tableau$diverging$temperature,
#'                                direction = 1,
#'                                start = 0.2,end = 0.8)

scale_color_tableau_gradient = function(palette = pal_tableau$sequential$blue_teal, direction = 1,
                                        na.value = "grey50", start = 0, end = 1,
                                        guide = "colourbar", ...) {
  manual_rescale = rescale_gradient(palette,direction,start,end)
  ggplot2::continuous_scale("colour",
                            palette = manual_rescale,
                            na.value = na.value,
                            guide = guide, ...)
}

