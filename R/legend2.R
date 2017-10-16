#' Draws a legend
#'
#' @param x A numeric value for the x coordinate of the top-left part of the
#' legend
#' @param y A numeric value for the y coordinate of the top-left part of the
#' legend
#' @param legend A numeric vector to appear in the legend.
#' @param col A vector of colors, if there are too few elements to fill the
#' legend, the elements in \code{col} are recycled.
#' @param locate A boolean, if TRUE, call the function \code{locator} to
#' indicate the top-left point of the legend
#' @param pos A character, by default \code{top-left}, but can be
#' \code{"top-right"}, \code{"bottom-left"} or \code{"bottom-right"},
#' used to indicate the position of the scale legend if \code{x, y} are not
#' indicated
#' @param n_round An integer indicating the number of significant digits to be
#' used, by default \code{0}.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = NULL}). If specified, a NA value will be add to
#' the lefend with the color corresponding.
#' @param postext A character defining the side of the legend text, by default
#' \code{left} but can be \code{right}
#' @param h A numeric expressing the height of one rectangle
#' in the legend
#' @param w A numeric expressing the width of the legend
#' @param tl A numeric expressing the length of the tick
#' @param s A numeric parameter expressing the space between the text and the
#' tick
#' @param ... if need to imput more text parameters for the legend
#'
#' @keywords internal
#' @noRd
square_legend <- function(x, y, legend, col, n_round = 0, col_na = NULL,
                          postext = "left",
                          h = 0.75, w = 0.75, tl = .2, s = .2, ...) {

  # size of the top character (width height)
  size_legend <- legend %>% as.character %>% tail(1) %>%
    strwidth()

  # define point of the legend
  if (postext == "left") {
    xleft <- x + size_legend + tl + s
  }
  if (postext == "right") {
    xleft <- x
  }
  xright <- xleft + w


  # define the y for rectangle legend
  col %<>% rev
  y1 <- y - (0:(length(legend) - 1)) * h

  # built the legend rectangles
  if(length(legend) > 12){
    y1 <- seq(y, tail(y1, 1), length.out = length(col))
  }

  for(i in seq_len(length(legend) - 1))
    rect(xleft, y1[i + 1], xright, y1[i], col = col[i], border = NA)
    rect(xleft, tail(y1, 1), xright, y1[1])

  # legend text and tick
  if(length(col) > 12){
    legend %<>% pretty()
    y1 <- seq(y, tail(y1, 1), length.out = length(legend))
  }

  # Define if segments should be on the left or the right
  if (postext == "left") {
    segments(xleft, y1, xleft - tl, y1)
  }
  if (postext == "right"){
    segments(xright, y1, xright + tl, y1)
  }

  # print legend text
  y2 <- seq(y, tail(y1, 1), length.out = length(legend))

  # If want NA, add a rectangle of the color NA
  if(length(col_na) != 0) {
    if(length(col) > 12){
      h <- (max(y1) - min(y1)) * 0.075
    } else {
      h <- y1[1] - y1[2]
    }
    rect(xleft, tail(y1, 1) - h , xright, tail(y1, 1) - 2 * h, col = col_na)
  }

  if (length(col_na) > 0){
    # define the y  with NA for the text
    y2 <- c(y2, y2[length(y2)] - h, y2[length(y2)] - 1.5 * h)

    # legend with NA on the left or right side
    if (postext == "left") {
      text(x + size_legend, y2,
           c(format(round(rev(legend),n_round), nsmall = n_round), "", "NA"),
           adj = 1, ...)
    }
    if (postext == "right") {
      text(xright + tl + s , y2,
           c(format(round(rev(legend),n_round), nsmall = n_round), "", "NA"),
           adj = 0, ...)
    }

  } else {
    if (postext == "left") {
      text(x + size_legend, y2,
           format(round(rev(legend),n_round), nsmall = n_round),
           adj = 1, ...)
    }
    if (postext == "right") {
      text(xright + tl + s , y2,
           format(round(rev(legend),n_round), nsmall = n_round),
           adj = 0, ...)
    }
  }
}

################################################################################
#' Draws a legend
#'
#' Draws a scale legend
#'
#' @param x A numeric value for the x coordinate of the top-left part of the
#' legend
#' @param y A numeric value for the y coordinate of the top-left part of the
#' legend
#' @param legend A character or expression vector to appear in the legend.
#' @param col A vector of colors, if there are too few elements to fill the
#' legend, the elements in \code{col} are recycled.
#' @param locate A boolean, if TRUE, call the function \code{locator} to
#' indicate the top-left point of the legend
#' @param pos A character, by default \code{top-left}, but can be
#' \code{"top-right"}, \code{"bottom-left"} or \code{"bottom-right"},
#' used to indicate the position of the scale legend if \code{x, y} are not
#' indicated
#' @param n_round An integer indicating the number of significant digits to be
#' used, by default \code{0}.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = NULL}). If specified, a NA value will be add to
#' the legend with the color corresponding.
#' @param postext A character defining the side of the legend text, by default
#' \code{left} but can be \code{right}
#' @param h A numeric expressing the height of one rectangle
#' in the legend
#' @param w A numeric expressing the width of the legend
#' @param tl A numeric expressing the length of the tick
#' @param s A numeric parameter expressing the space between the text and the
#' tick
#' @param ... if need to imput more text parameters for the legend
#'
#' @details The number of rectangle in the scale legend is calculate with the
#' number of color in the vector \code{col}.\cr
#' \cr If arguments \code{x,y} are not filled, the location may also be
#' specified by setting the parameter \code{pos} to a keyword form the list:
#' \code{"top-left"}, \code{"top-right"}, \code{"bottom-left"} or
#' \code{"bottom-right"}. This places the legend on the inside of the plot
#' frame at the giver location. \cr
#' \cr Note that a call to the function \code{locator(1)} can be used via
#' setting the parameter \code{locate} to TRUE in place of the \code{x} and
#' \code{y} arguments. \cr
#' \cr If the length of the vector \code{legend} is higher than 12, the axis
#' legend will be calculate with the R break algorithm as implemented in pretty.
#'
#' @examples
#' library(gdpm)
#' library(magrittr)
#' library(gadmVN)
#'
#' # dengue data
#' dengue <- getid(dengue, from = 1992, to = 2010)
#' # geographic data
#' map <- gadmVN::gadm(date = 1992, merge_hanoi = TRUE)
#' # preparation of the data
#' library(dplyr)
#' dengue_0993  <- filter(dengue, year == 1993, month == "September")
#' dengue_0993 <- select(dengue_0993, province, contains("incidence"))
#'
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' # By default, col = heat.colors(6) in choromap function
#' legend2(97, 22.5 ,a ,col = heat.colors(6))
#'
#' # By default, the legend is on the top left of the figure if x and y are not
#' # filled, but the position can be easily change by using the parameters pos:
#' # top left
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' legend2(legend = a, col = heat.colors(6))
#' # top right
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' legend2(legend = a, col = heat.colors(6), pos = "top-right")
#' # bottom left
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' legend2(legend = a, col = heat.colors(6), pos = "bottom-left")
#' # bottom right
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' legend2(legend = a, col = heat.colors(6), pos = "bottom-right")
#'
#' # By default, the legend text is on the left of the scale, but the
#' # position can be easily change by using the parameters postext:
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' legend2(legend = a, col = heat.colors(6), pos = "bottom-right",
#'          postext = "right")
#'
#' # Printing the color of the missing values:
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' legend2(legend = a, col = heat.colors(6), col_na = "grey")
#'
#' # Changing the number of decimal:
#' a <- choromap(dengue_0993, map,
#'                fixedBreaks = c(0, 0.5, 5, 50, 150, 1000, 2000))
#' legend2(legend = a, col = heat.colors(6), col_na = "grey", n_round = 1)
#'
#' # Changing the text parameters:
#' a <- choromap(dengue_0993, map,
#'                fixedBreaks = c(0, 0.5, 5, 50, 150, 1000, 2000))
#' legend2(legend = a, col = heat.colors(6), col_na = "grey", n_round = 2,
#'          cex = 0.8)
#'
#' # Changing the parameters of the scale:
#' a <- choromap(dengue_0993, map,
#'                fixedBreaks = c(0, 0.5, 5, 50, 150, 1000, 2000))
#' legend2(legend = a, col = heat.colors(6), col_na = "grey",
#'          h = 0.5, w = 0.4, cex = 0.6)
#'
#' # Using the locator to choose where to print the legend
#'\dontrun{
#' choromap(dengue_0993, map)
#' legend2(legend = a, col = heat.colors(6), locate = TRUE)
#'}
#'
#'
#' @export
legend2 <- function(x, y, legend, col, locate = FALSE, pos = "top-left",
                    n_round = 0, col_na = NULL, postext = "left", h = 0.75,
                    w = 0.75, tl = .2, s = .2, ...){

  # Tests
  pos_text <- c("left", "right")
  if(!is.element(postext, pos_text)){
    stop("The parameters 'postext' can only contain: 'left' or 'right'")
  }
  if(!is.logical(locate)){
    stop("The parameters 'locate' can only be a logical: 'TRUE' or 'FALSE'")
  }

  # Graphic paramaters
  omar <- par("mar")
  par(mar = c(2,2,2,1))
  on.exit(par(mar = omar))

  # size of the top character (width height)
  size_legend <- legend %>% as.character %>% tail(1) %>%
    strwidth()

  if (missing(x) & missing(y) & locate == FALSE){

    # Test
    pos_legend <- c("top-left", "top-right", "bottom-left", "bottom-right")
    if(!is.element(pos, pos_legend)){
      stop("The parameters 'pos' can only contain: 'top-left', 'top-right',
           'bottom-left' or 'bottom-right'")
    }

    # Graphical parameters
    usr <- par("usr")
    xr <- (usr[2] - usr[1])/27
    yr <- (usr[4] - usr[3])/27
    xlim <- c(usr[1] + xr, usr[2] - xr)
    ylim <- c(usr[3] + yr, usr[4] - yr)

    if (pos == "top-left"){
      x <- xlim[1]
      y <- ylim[2]
    }
    if (pos == "top-right"){
      x <- xlim[2] - w - size_legend - tl -s
      y <- ylim[2]
    }
    if (pos == "bottom-left"){
      x <- xlim[1]
      y <- ylim[1] + ((length(legend)-1)* h + 2 * h)
    }
    if (pos == "bottom-right"){
      x <- xlim[2] - w - size_legend -tl - s
      y <- ylim[1] + ((length(legend)-1)* h + 2 * h)
    }

    }

  if (locate == TRUE){
    coordinates <- locator(1)
    x = coordinates$x
    y = coordinates$y
  }

  square_legend(x, y, legend = legend, col = col, n_round = n_round,
                col_na = col_na, postext = postext, h = h, w = w, tl = tl,
                s = s, ...)
}
