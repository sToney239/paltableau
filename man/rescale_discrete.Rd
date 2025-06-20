% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescale_function.R
\name{rescale_discrete}
\alias{rescale_discrete}
\title{Rescale function for discrete}
\usage{
rescale_discrete(palette, discreted, direction, start, end)
}
\arguments{
\item{palette}{Palette from "paltableau" package. \if{html}{\out{<br>}} Try to type "pal_tableau$" and wait for hint of "regular", "sequential" or "diverging",\if{html}{\out{<br>}} select one, and type "$" to choose the ultimate palette.}

\item{discreted}{A logical value indicating whether the input color value vector is already discrete or need further break funciton. \cr
The default is TRUE, it assumes that the input is already discrete and does not require further discretization.}

\item{direction}{Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.}

\item{start}{The (corrected) color range in [0,1]⁠ at which the color map starts.}

\item{end}{The (corrected) color range in [0,1]⁠ at which the color map ends.}
}
\description{
Rescale function for discrete
}
\keyword{internal}
