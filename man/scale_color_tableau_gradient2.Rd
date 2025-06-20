% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_tableau_gradient2.R
\name{scale_color_tableau_gradient2}
\alias{scale_color_tableau_gradient2}
\title{Tableau color scales (low-mid-high)}
\usage{
scale_color_tableau_gradient2(
  palette = pal_tableau$diverging$classic_red_blue,
  midpoint = 0,
  direction = 1,
  na.value = "grey50",
  start = 0,
  end = 1,
  transform = "identity",
  guide = "colourbar",
  ...
)
}
\arguments{
\item{palette}{Palette from "paltableau" package. \if{html}{\out{<br>}} Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",\if{html}{\out{<br>}} select one, and type "$" to choose the ultimate palette.}

\item{midpoint}{The midpoint (in data value) of the diverging scale.\if{html}{\out{<br>}}Defaults to 0.}

\item{direction}{Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.}

\item{na.value}{Colour to use for missing values}

\item{start}{The (corrected) color range in [0,1]⁠ at which the color map starts.}

\item{end}{The (corrected) color range in [0,1]⁠ at which the color map ends.}

\item{transform}{For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".\if{html}{\out{<br>}}A transformation object bundles together a transform, its inverse, and methods for generating breaks and labels. Transformation objects are defined in the scales package, and are called ⁠\verb{transform_<name>}⁠. If transformations require arguments, you can call them from the scales package, e.g \code{\link[scales]{transform_boxcox}}... You can create your own transformation with \code{\link[scales]{new_transform}}.}

\item{guide}{Type of legend. Use "colourbar" for continuous colour bar, or "legend" for discrete colour legend.}

\item{...}{Other arguments passed on to \code{\link[ggplot2]{continuous_scale}}.}
}
\description{
Tableau color scales (low-mid-high)
}
\examples{
library(ggplot2)
library(paltableau)
ggplot(iris)+
  geom_point(aes(Sepal.Length, Sepal.Width, color = Sepal.Length))+
  scale_color_tableau_gradient2(palette = pal_tableau$diverging$red_blue_white,
                                direction = 1,
                                midpoint = 6)

}
