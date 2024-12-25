# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Tableau fill scales (discrete)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}.
#'
#' @examples ggplot(iris)+
#'  geom_point(aes(Sepal.Length,Sepal.Width,color = Species))+
#'  scale_color_tableau_discrete(palette = pal_tableau$diverging$orange_blue,
#'                               direction = -1,
#'                               start = 0.2,end = 0.8)

scale_fill_tableau_discrete = function(palette = pal_tableau$regular$`Tableau 10`, direction = 1, na.value = "grey50",
                                       start = 0, end = 1, ...) {
  if(palette$type[[1]] != "regular") {
    the_pal =  palette
    if(direction != 1) the_pal = rev(the_pal)
    manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(1:x,to = c(start,end),from = c(1,x)))
  } else {
    manual_rescale = \(x) palette
  }

  discrete_scale("fill",
                 palette = manual_rescale,
                 na.value = na.value,
                 ...)
}
#' Tableau color scales (discrete)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}.
#'
#' @examples ggplot(iris)+
#'  geom_col(aes(Sepal.Length,Sepal.Width,fill = Species))+
#'  scale_fill_tableau_discrete(palette = pal_tableau$diverging$orange_blue,
#'                               direction = -1,
#'                               start = 0.2,end = 0.8)

scale_color_tableau_discrete = function(palette = pal_tableau$regular$`Tableau 10`, direction = 1, na.value = "grey50",
                                       start = 0, end = 1,
                                       ...) {
  if(palette$type[[1]] != "regular") {
    the_pal =  palette
    if(direction != 1) the_pal = rev(the_pal)
    manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(1:x,to = c(start,end),from = c(1,x)))
  } else {
    manual_rescale = \(x) palette
  }

  discrete_scale("color",
                 palette = manual_rescale,
                 na.value = na.value,
                 ...)
}


#' Tableau fill scales (gradient)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param guide Type of legend. Use 'colourbar' for continuous colour bar, or 'legend' for discrete colour legend.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{continuous_scale}}.
#'
#' @examples ggplot(iris)+
#'   geom_col(aes(Sepal.Length,Sepal.Width,fill = Sepal.Length))+
#'   scale_fill_tableau_gradient(palette = pal_tableau$diverging$temperature,
#'                               direction = 1,
#'                               start = 0.2,end = 0.8)

scale_fill_tableau_gradient = function(palette = pal_tableau$sequential$blue_teal,
                                       direction = 1, na.value = "grey50",
                                       start = 0, end = 1,
                                       guide = "colourbar",  ...) {
  the_pal =  palette
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("fill",
                   palette = manual_rescale,# scales::gradient_n_pal(colours = the_pal),
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
#'
#' @examples ggplot(iris)+
#'   geom_point(aes(Sepal.Length,Sepal.Width,color = Sepal.Width))+
#'   scale_color_tableau_gradient(palette = pal_tableau$diverging$temperature,
#'                                direction = 1,
#'                                start = 0.2,end = 0.8)

scale_color_tableau_gradient = function(palette = pal_tableau$sequential$blue_teal, direction = 1,
                                        na.value = "grey50", start = 0, end = 1,
                                        guide = "colourbar", ...) {
  the_pal = palette
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("colour",
                   palette = manual_rescale,# scales::gradient_n_pal(colours = the_pal),
                   na.value = na.value,
                   guide = guide, ...)
}


#' Tableau fill scales (binned)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends
#' @param breaks The breaks for bins
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
#'
#' @examples ggplot(iris)+
#'   geom_col(aes(Sepal.Length,Sepal.Width,fill = Sepal.Length))+
#'   scale_fill_tableau_binned_gradient(palette = pal_tableau$diverging$temperature,
#'                                      direction = 1,
#'                                      n.breaks = 7,
#'                                      start = 0.2,end = 0.8)

scale_fill_tableau_binned_gradient = function(palette = pal_tableau$sequential$blue_teal,
                                              na.value = "grey50",
                                              direction = 1,start = 0, end = 1,breaks = NULL,
                                              ...) {
  the_pal = palette
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  if (!is.null(breaks)) {
    manual_rescale2 = \(x) manual_rescale((seq_len(length(breaks)-1)-1)/(length(breaks)-2))
  } else {
    breaks = waiver()
    manual_rescale2 = manual_rescale
  }
  binned_scale("fill",
               palette=  manual_rescale,# scales::gradient_n_pal(colours = the_pal),
               na.value = na.value,
               breaks = breaks,
               guide = guide_bins(theme = theme(legend.ticks = element_blank(),legend.axis.line = element_line(color = "transparent"))),
               ...,
               show.limits = TRUE,
               oob = scales::squish)
}



#' Tableau color scales (binned)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param na.value Colour to use for missing values
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param breaks The breaks for bins
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
#'
#' @examples ggplot(iris)+
#'   geom_point(aes(Sepal.Length,Sepal.Width,color = Sepal.Length))+
#'   scale_color_tableau_binned_gradient(palette = pal_tableau$diverging$temperature,
#'                                       direction = 1,
#'                                       n.breaks = 7,
#'                                       start = 0.2,end = 0.8)

scale_color_tableau_binned_gradient = function(palette = pal_tableau$sequential$blue_teal, na.value = "grey50",
                                               direction=1,start = 0, end = 1,breaks = NULL, ...) {
  the_pal = palette
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  if (!is.null(breaks)) {
    manual_rescale2 = \(x) manual_rescale((seq_len(length(breaks)-1)-1)/(length(breaks)-2))
  } else {
    breaks = waiver()
    manual_rescale2 = manual_rescale
  }
  binned_scale("color",
               palette=  manual_rescale,# scales::gradient_n_pal(colours = the_pal),
               na.value = na.value,
               breaks = breaks,
               guide = guide_bins(theme = theme(legend.ticks = element_blank(),legend.axis.line = element_line(color = "transparent"))),
               ...,
               show.limits = TRUE,
               oob = scales::squish)
}


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
#'
#' @examples ggplot(iris)+
#'   geom_col(aes(Sepal.Length,Sepal.Width,fill = Sepal.Length))+
#'   scale_fill_tableau_gradient2(palette = pal_tableau$diverging$temperature,
#'                                midpoint = 6,
#'                                direction = 1)

scale_fill_tableau_gradient2 = function(palette = pal_tableau$diverging$classic_red_blue,
                                        midpoint = 0,
                                        direction = 1,na.value = "grey50",
                                        start = 0, end = 1,
                                        transform = "identity", guide = "colourbar",...) {
  the_pal = palette
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x <- ifelse(x < 0.5,
                scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
                ifelse(x > 0.5,
                       scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
                       0.5))
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  # manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("fill",
                   palette =  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
                   na.value = na.value,
                   transform = transform,
                   guide = guide, ...,
                   rescaler = ggplot2:::mid_rescaler(mid = midpoint, transform = transform))
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
#'
#' @examples ggplot(iris)+
#'   geom_point(aes(Sepal.Length,Sepal.Width,color = Sepal.Length))+
#'   scale_color_tableau_gradient2(palette = pal_tableau$diverging$red_blue_white,
#'                                 direction = 1,
#'                                 midpoint = 6)
#'

scale_color_tableau_gradient2 = function(palette = pal_tableau$diverging$classic_red_blue, midpoint = 0, direction = 1,space = "Lab", na.value = "grey50",
                                         start = 0, end = 1,
                                         transform = "identity", guide = "colourbar",  ...) {
  the_pal = palette
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x <- ifelse(x < 0.5,
                scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
                ifelse(x > 0.5,
                       scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
                       0.5))
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  # manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("color",
                   palette =  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
                   na.value = na.value,
                   transform = transform,
                   guide = guide, ...,
                   rescaler = ggplot2:::mid_rescaler(mid = midpoint, transform = transform))
}



#' Tableau fill scales (binned & low-mid-high)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
#' @param na.value Colour to use for missing values
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param breaks The breaks for bins
#' @param transform For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".<br>A transformation object bundles together a transform, its inverse, and methods for generating breaks and labels. Transformation objects are defined in the scales package, and are called ⁠`transform_<name>`⁠. If transformations require arguments, you can call them from the scales package, e.g \code{\link[scales]{transform_boxcox}}... You can create your own transformation with \code{\link[scales]{new_transform}}.
#'
#' @examples ggplot(iris)+
#'  geom_col(aes(Sepal.Length,Sepal.Width,fill = Sepal.Length))+
#'  scale_fill_tableau_binned_gradient2(palette = pal_tableau$diverging$temperature)
#'
#'

scale_fill_tableau_binned_gradient2 = function(palette = pal_tableau$diverging$red_blue_white,
                                               na.value = "grey50",
                                               direction = 1,start = 0, end = 1,
                                               breaks = NULL,
                                               transform = "identity",...) {
  the_pal = palette
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x <- ifelse(x < 0.5,
                scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
                ifelse(x > 0.5,
                       scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
                       0.5))
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  if (!is.null(breaks)) {
    manual_rescale = \(x) manual_rescale2((seq_len(length(breaks)-1)-1)/(length(breaks)-2))
  } else {
    breaks = waiver()
    manual_rescale = manual_rescale2
  }

  binned_scale("fill",
               palette=  manual_rescale,
               na.value = na.value,
               transform = transform,
               show.limits = TRUE,
               breaks = breaks,
               guide = guide_bins(theme = theme(legend.ticks = element_blank(),legend.axis.line = element_line(color = "transparent"))),
               ...,
               oob = scales::squish)
}


#' Tableau color scales (binned & low-mid-high)
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{binned_scale}}.
#' @param na.value Colour to use for missing values
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @param breaks The breaks for bins
#' @param transform For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".<br>A transformation object bundles together a transform, its inverse, and methods for generating breaks and labels. Transformation objects are defined in the scales package, and are called ⁠`transform_<name>`⁠. If transformations require arguments, you can call them from the scales package, e.g \code{\link[scales]{transform_boxcox}}... You can create your own transformation with \code{\link[scales]{new_transform}}.
#'
#' @examples ggplot(iris)+
#'  geom_point(aes(Sepal.Length,Sepal.Width,color = Sepal.Length))+
#'  scale_color_tableau_binned_gradient2(palette = pal_tableau$diverging$red_blue_white, direction = 1)
#'

scale_color_tableau_binned_gradient2 = function(palette = pal_tableau$diverging$red_blue_white,
                                                na.value = "grey50",
                                                direction=1,start = 0, end = 1,
                                                breaks = NULL,
                                                ...,
                                                transform = "identity") {
  the_pal = palette
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x <- ifelse(x < 0.5,
                scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
                ifelse(x > 0.5,
                       scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
                       0.5))
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  if (!is.null(breaks)) {
    manual_rescale = \(x) manual_rescale2((seq_len(length(breaks)-1)-1)/(length(breaks)-2))
  } else {
    breaks = waiver()
    manual_rescale = manual_rescale2
  }

  binned_scale("color",
               palette=  manual_rescale,
               na.value = na.value,
               transform = transform,
               show.limits = TRUE,
               breaks = breaks,
               guide = guide_bins(theme = theme(legend.ticks = element_blank(),legend.axis.line = element_line(color = "transparent"))),
               ...,
               oob = scales::squish)

}


