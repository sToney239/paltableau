#' Binned gradient fill scales with Tableau palettes
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends
#' @param breaks The breaks for bins
#' @param guide Type of legend. Use guide names like 'coloursteps' or 'bins', or use 	a function used to create a guide. See \code{\link[ggplot2]{guides}} for more information.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
#' @export
#'
#' @examples library(ggplot2)
#' library(paltableau)
#' ggplot(iris)+
#'   geom_col(aes(Sepal.Length, Sepal.Width, fill = Sepal.Length))+
#'   scale_fill_tableau_steps(palette = pal_tableau$diverging$temperature,
#'                                       direction = 1,
#'                                       n.breaks = 7,
#'                                       guide = guide_bins(theme = theme(legend.ticks = element_blank(),legend.axis.line = element_line(color = "transparent"))),
#'                                       start = 0.2,end = 0.8)
scale_fill_tableau_steps = function(palette = pal_tableau$sequential$blue_teal,
                                              na.value = "grey50",
                                              direction = 1,start = 0, end = 1,breaks = NULL,
                                              guide = "coloursteps",
                                              ...) {
  the_pal = palette
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  if (!is.null(breaks)) {
    manual_rescale2 = \(x) manual_rescale((seq_len(length(breaks)-1)-1)/(length(breaks)-2))
  } else {
    breaks = ggplot2::waiver()
    manual_rescale2 = manual_rescale
  }
  ggplot2::binned_scale("fill",
                        palette=  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
                        na.value = na.value,
                        breaks = breaks,
                        guide = guide,
                        ...,
                        show.limits = TRUE,
                        oob = scales::squish)
}







#' Binned gradient color scales with Tableau palettes
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param breaks The breaks for bins
#' @param guide Type of legend. Use guide names like 'coloursteps' or 'bins', or use 	a function used to create a guide. See \code{\link[ggplot2]{guides}} for more information.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
#' @export
#'
#' @examples library(ggplot2)
#' library(paltableau)
#' ggplot(iris)+
#'   geom_point(aes(Sepal.Length, Sepal.Width, color = Sepal.Length))+
#'   scale_color_tableau_steps(palette = pal_tableau$diverging$temperature,
#'                                       direction = 1,
#'                                       n.breaks = 7,
#'                                       guide = guide_bins(theme = theme(legend.ticks = element_blank(),legend.axis.line = element_line(color = "transparent"))),
#'                                       start = 0.2,end = 0.8)

scale_color_tableau_steps = function(palette = pal_tableau$sequential$blue_teal, na.value = "grey50",
                                               direction=1,start = 0, end = 1,breaks = NULL, guide = "coloursteps",...) {
  the_pal = palette
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  if (!is.null(breaks)) {
    manual_rescale2 = \(x) manual_rescale((seq_len(length(breaks)-1)-1)/(length(breaks)-2))
  } else {
    breaks = ggplot2::waiver()
    manual_rescale2 = manual_rescale
  }
  ggplot2::binned_scale("color",
                        palette=  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
                        na.value = na.value,
                        breaks = breaks,
                        guide = guide,
                        ...,
                        show.limits = TRUE,
                        oob = scales::squish)
}



#' Binned gradient fill scales with Tableau palettes
#' @description
#' `r lifecycle::badge("deprecated")`\cr
#' Please Use \code{\link[paltableau]{scale_fill_tableau_steps}}.
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends
#' @param breaks The breaks for bins
#' @param guide Type of legend. Use guide names like 'coloursteps' or 'bins', or use 	a function used to create a guide. See \code{\link[ggplot2]{guides}} for more information.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
scale_fill_tableau_binned_gradient <- function(palette = pal_tableau$sequential$blue_teal,
                                               na.value = "grey50",
                                               direction = 1,start = 0, end = 1,breaks = NULL,
                                               guide = "coloursteps",
                                               ...) {
  lifecycle::deprecate_warn("0.1.0", "scale_fill_tableau_binned_gradient()", "scale_fill_tableau_steps()")
  scale_fill_tableau_steps(palette = pal_tableau$sequential$blue_teal,
                           na.value = "grey50",
                           direction = 1,start = 0, end = 1,breaks = NULL,
                           guide = "coloursteps",
                           ...)
}


#' Binned gradient color scales with Tableau palettes
#' @description
#' `r lifecycle::badge("deprecated")`\cr
#' Please use \code{\link[paltableau]{scale_color_tableau_steps}}.
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param breaks The breaks for bins
#' @param guide Type of legend. Use guide names like 'coloursteps' or 'bins', or use 	a function used to create a guide. See \code{\link[ggplot2]{guides}} for more information.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
scale_color_tableau_binned_gradient <- function(palette = pal_tableau$sequential$blue_teal,
                                               na.value = "grey50",
                                               direction = 1,start = 0, end = 1,breaks = NULL,
                                               guide = "coloursteps",
                                               ...) {
  lifecycle::deprecate_warn("0.1.0", "scale_color_tableau_binned_gradient()", "scale_color_tableau_steps()")
  scale_color_tableau_steps(palette = pal_tableau$sequential$blue_teal,
                            na.value = "grey50",
                            direction = 1,start = 0, end = 1,breaks = NULL,
                            guide = "coloursteps",
                            ...)
}
