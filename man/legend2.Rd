% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legend2.R
\name{legend2}
\alias{legend2}
\title{Draws a legend}
\usage{
legend2(x, y, legend, col, locate = FALSE, pos = "top-left",
  n_round = 0, col_na = NULL, size_na = 0.075, postext = "left",
  h = 0.75, w = 0.75, tl = 0.2, s = 0.2, ...)
}
\arguments{
\item{x}{A numeric value for the x coordinate of the top-left part of the
legend}

\item{y}{A numeric value for the y coordinate of the top-left part of the
legend}

\item{legend}{A character or expression vector to appear in the legend.}

\item{col}{A vector of colors, if there are too few elements to fill the
legend, the elements in \code{col} are recycled.}

\item{locate}{A boolean, if TRUE, call the function \code{locator} to
indicate the top-left point of the legend}

\item{pos}{A character, by default \code{top-left}, but can be
\code{"top-right"}, \code{"bottom-left"} or \code{"bottom-right"},
used to indicate the position of the scale legend if \code{x, y} are not
indicated}

\item{n_round}{An integer indicating the number of significant digits to be
used, by default \code{0}.}

\item{col_na}{the color with which to represent the missing values
(by default \code{col_na = NULL}). If specified, a NA value will be add to
the legend with the color corresponding.}

\item{size_na}{A numeric value expressing the size of the rectangle for the
missing value, expressed in a proportion of the scale legend (between 0 and
1, by default \code{size_na = 0.075})}

\item{postext}{A character defining the side of the legend text, by default
\code{left} but can be \code{right}}

\item{h}{A numeric expressing the height of one rectangle
in the legend}

\item{w}{A numeric expressing the width of the legend}

\item{tl}{A numeric expressing the length of the tick}

\item{s}{A numeric parameter expressing the space between the text and the
tick}

\item{...}{if need to imput more text parameters for the legend}
}
\description{
Draws a scale legend
}
\details{
The number of rectangle in the scale legend is calculate with the
number of color in the vector \code{col}.\cr
\cr If arguments \code{x,y} are not filled, the location may also be
specified by setting the parameter \code{pos} to a keyword form the list:
\code{"top-left"}, \code{"top-right"}, \code{"bottom-left"} or
\code{"bottom-right"}. This places the legend on the inside of the plot
frame at the giver location. \cr
\cr Note that a call to the function \code{locator(1)} can be used via
setting the parameter \code{locate} to TRUE in place of the \code{x} and
\code{y} arguments. \cr
\cr If the length of the vector \code{legend} is higher than 12, the axis
legend will be calculate with the R break algorithm as implemented in pretty.
}
\examples{
library(gdpm)
library(magrittr)
library(gadmVN)

# dengue data
dengue <- getid(dengue, from = 1992, to = 2010)
# geographic data
map <- gadmVN::gadm(date = 1992, merge_hanoi = TRUE) \%>\% sf::as_Spatial()
# preparation of the data
library(dplyr)
dengue_0993  <- filter(dengue, year == 1993, month == "September")
dengue_0993 <- select(dengue_0993, province, contains("incidence"))

a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
# By default, col = heat.colors(6) in choromap function
legend2(97, 22.5 ,a ,col = heat.colors(6))

# By default, the legend is on the top left of the figure if x and y are not
# filled, but the position can be easily change by using the parameters pos:
# top left
a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
legend2(legend = a, col = heat.colors(6))
# top right
a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
legend2(legend = a, col = heat.colors(6), pos = "top-right")
# bottom left
a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
legend2(legend = a, col = heat.colors(6), pos = "bottom-left")
# bottom right
a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
legend2(legend = a, col = heat.colors(6), pos = "bottom-right")

# By default, the legend text is on the left of the scale, but the
# position can be easily change by using the parameters postext:
a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
legend2(legend = a, col = heat.colors(6), pos = "bottom-right",
         postext = "right")

# Printing the color of the missing values:
a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
legend2(legend = a, col = heat.colors(6), col_na = "grey")

# Changing the number of decimal:
a <- choromap(dengue_0993, map,
               fixedBreaks = c(0, 0.5, 5, 50, 150, 1000, 2000))
legend2(legend = a, col = heat.colors(6), col_na = "grey", n_round = 1)

# Changing the text parameters:
a <- choromap(dengue_0993, map,
               fixedBreaks = c(0, 0.5, 5, 50, 150, 1000, 2000))
legend2(legend = a, col = heat.colors(6), col_na = "grey", n_round = 2,
         cex = 0.8)

# Changing the parameters of the scale:
a <- choromap(dengue_0993, map,
               fixedBreaks = c(0, 0.5, 5, 50, 150, 1000, 2000))
legend2(legend = a, col = heat.colors(6), col_na = "grey",
         h = 0.5, w = 0.4, cex = 0.6)

# Using the locator to choose where to print the legend
\dontrun{
choromap(dengue_0993, map)
legend2(legend = a, col = heat.colors(6), locate = TRUE)
}


}
