#' Tableau fill scales (discrete)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param discreted A logical value indicating whether the input color value vector is already discrete or need further break funciton. \cr
#' The default is TRUE, it assumes that the input is already discrete and does not require further discretization.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}.
#' @export
#'
#' @examples library(ggplot2)
#' library(paltableau)
#' ggplot(iris)+
#'   geom_point(aes(Sepal.Length, Sepal.Width, color = Species))+
#'   scale_color_tableau_discrete(palette = pal_tableau$diverging$orange_blue,
#'                                direction = -1,
#'                                start = 0.2, end = 0.8)

scale_fill_tableau_discrete = function(palette = pal_tableau$regular$`Tableau 10`,
                                       discreted = TRUE,
                                       direction = 1, na.value = "grey50",
                                       start = 0, end = 1, ...) {
  manual_rescale = rescale_discrete(palette, discreted, direction, start, end)

  ggplot2::discrete_scale("fill",
                          palette = manual_rescale,
                          na.value = na.value,
                          ...)
}
#' Tableau color scales (discrete)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param discreted A logical value indicating whether the input color value vector is already discrete or need further break funciton. \cr
#' The default is TRUE, it assumes that the input is already discrete and does not require further discretization.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}.
#' @export
#'
#' @examples library(ggplot2)
#' ggplot(iris)+
#'   geom_col(aes(Sepal.Length, Sepal.Width, fill = Species))+
#'   scale_fill_tableau_discrete(palette = pal_tableau$diverging$orange_blue,
#'                               direction = -1,
#'                               start = 0.2,end = 0.8)


scale_color_tableau_discrete = function(palette = pal_tableau$regular$`Tableau 10`,
                                        discreted = TRUE,
                                        direction = 1, na.value = "grey50",
                                        start = 0, end = 1,
                                        ...) {
  manual_rescale = rescale_discrete(palette, discreted, direction, start, end)

  ggplot2::discrete_scale("color",
                          palette = manual_rescale,
                          na.value = na.value,
                          ...)
}




