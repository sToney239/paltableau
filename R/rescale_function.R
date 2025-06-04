#' Rescale function for discrete
#'
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param discreted A logical value indicating whether the input color value vector is already discrete or need further break funciton. \cr
#' The default is TRUE, it assumes that the input is already discrete and does not require further discretization.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @keywords internal
rescale_discrete = function(palette, discreted, direction, start, end) {
  if(direction != 1) palette = rev(palette)
  if(!discreted) {
    the_pal =  palette
    manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(1:x,to = c(start,end),from = c(1,x)))
  } else {
    manual_rescale = \(x) palette
  }
  return(manual_rescale)
}

#' Rescale function for gradient
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @keywords internal
rescale_gradient = function(palette,direction,start,end) {
  the_pal = palette
  if(direction == 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  return(manual_rescale)
}


#' Rescale function for gradient2
#' @param palette Palette from "paltableau" package. <br> Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",<br> select one, and type "$" to choose the ultimate palette.
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param start The (corrected) color range in \[0,1\]⁠ at which the color map starts.
#' @param end The (corrected) color range in \[0,1\]⁠ at which the color map ends.
#' @keywords internal
rescale_gradient2 = function(palette,direction,start,end) {
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
  return(manual_rescale2)
}


#' internal version of mid_rescaler from ggplot2 package
#' @param mid The midpoint (in data value) of the diverging scale.<br>Defaults to 0.
#' @param transform For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".<br>A transformation object bundles together a transform, its inverse, and methods for generating breaks and labels. Transformation objects are defined in the scales package, and are called ⁠`transform_<name>`⁠. If transformations require arguments, you can call them from the scales package, e.g \code{\link[scales]{transform_boxcox}}... You can create your own transformation with \code{\link[scales]{new_transform}}.
#' @keywords internal
mid_rescaler = function (mid, transform) {
  transform <- scales::as.trans(transform)
  trans_mid <- transform$transform(mid)
  # check_transformation(mid, trans_mid, transform$name)
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, trans_mid)
  }
}



