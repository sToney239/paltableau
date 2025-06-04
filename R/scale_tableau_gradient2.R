#' Tableau fill scales (low-mid-high)
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param midpoint The midpoint (in data value) of the diverging scale.<br>Defaults to 0.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param transform For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".<br>A transformation object bundles together a transform, its inverse, and methods for generating breaks and labels. Transformation objects are defined in the scales package, and are called ⁠`transform_<name>`⁠. If transformations require arguments, you can call them from the scales package, e.g \code{\link[scales]{transform_boxcox}}... You can create your own transformation with \code{\link[scales]{new_transform}}.
#' @param guide Type of legend. Use "colourbar" for continuous colour bar, or "legend" for discrete colour legend.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{continuous_scale}}.
#' @export
#'
#' @examples library(ggplot2)
#' library(paltableau)
#' ggplot(iris)+
#'   geom_col(aes(Sepal.Length, Sepal.Width, fill = Sepal.Length))+
#'   scale_fill_tableau_gradient2(palette = pal_tableau$diverging$temperature,
#'                                midpoint = 6,
#'                                direction = 1)

scale_fill_tableau_gradient2 = function(palette = pal_tableau$diverging$classic_red_blue,
                                        midpoint = 0,
                                        direction = 1,na.value = "grey50",
                                        start = 0, end = 1,
                                        transform = "identity", guide = "colourbar",...) {
  manual_rescale2 = rescale_gradient2(palette,direction,start,end)
  ggplot2::continuous_scale("fill",
                            palette =  manual_rescale2,
                            na.value = na.value,
                            transform = transform,
                            guide = guide, ...,
                            rescaler = mid_rescaler(mid = midpoint, transform = transform))
}

#' Tableau color scales (low-mid-high)
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param midpoint The midpoint (in data value) of the diverging scale.<br>Defaults to 0.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param transform For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".<br>A transformation object bundles together a transform, its inverse, and methods for generating breaks and labels. Transformation objects are defined in the scales package, and are called ⁠`transform_<name>`⁠. If transformations require arguments, you can call them from the scales package, e.g \code{\link[scales]{transform_boxcox}}... You can create your own transformation with \code{\link[scales]{new_transform}}.
#' @param guide Type of legend. Use "colourbar" for continuous colour bar, or "legend" for discrete colour legend.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{continuous_scale}}.
#' @export
#'
#' @examples library(ggplot2)
#' library(paltableau)
#' ggplot(iris)+
#'   geom_point(aes(Sepal.Length, Sepal.Width, color = Sepal.Length))+
#'   scale_color_tableau_gradient2(palette = pal_tableau$diverging$red_blue_white,
#'                                 direction = 1,
#'                                 midpoint = 6)
#'

scale_color_tableau_gradient2 = function(palette = pal_tableau$diverging$classic_red_blue, midpoint = 0, direction = 1, na.value = "grey50",
                                         start = 0, end = 1,
                                         transform = "identity", guide = "colourbar",  ...) {
  manual_rescale2 = rescale_gradient2(palette,direction,start,end)
  ggplot2::continuous_scale("color",
                            palette =  manual_rescale2,
                            na.value = na.value,
                            transform = transform,
                            guide = guide, ...,
                            rescaler = mid_rescaler(mid = midpoint, transform = transform))
}
