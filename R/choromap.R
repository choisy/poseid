#' Draws a choropleth map with one unique value
#'
#' This function draws a choropleth map when all the provinces or regions have
#' the same value.
#'
#' @param df an object of class "SpatialPolygonsDataFrame" containing also
#' the value to represent
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param n_round integer indicating the number of significant digits to be used
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value, returned invisibly.
#'
#' @keywords internal
#' @noRd
choropleth_v1 <- function (df, col = heat.colors(1), col_na = "grey",
                           n_round = 0){

  # value
  value <- df@data %>%
    select_if(is.numeric) %>%
    unlist %>%  as.vector

  # define the color and the class intervals
  pal <-  col
  pal <- pal[1]

  pal2 <-  rep(pal, length(value))
  pal2[is.na(value)] <- col_na
  classint <- na.omit(unique(value))

  # draw a choropleth map
  sp::plot(df, col = pal2)

  # print a legend
  legend <- rep(classint,2) %>% round(n_round)
  attr(legend, "colors") <- pal
  invisible(legend)
}

################################################################################
#' Draws a choropleth map with multiple value
#'
#' This function draws a choropleth map with at least two differents value. Use
#' the package \code{classInt} to specify the style and the number of breaks.
#'
#' @param df an object of class "SpatialPolygonsDataFrame" containing also
#' the value to represent
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param n a numeric indicating the number of intervals to represent the data
#' (by default, \code{n = 6})
#' @param style a character value issued from the \code{classint} package, and
#' used to select a method for the different way of calculating the intervals
#' (by default \code{style = "quantile"})
#' @param distrib if TRUE, print on the map, the distribution of the values by
#' intervals
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param pos_brks if TRUE, the breaks values will all be positive, the first
#' break will be superior or equal to zero, by default (\code{TRUE}). If false,
#' allows negative value for breaks
#' @param n_round integer indicating the number of significant digits to be used
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value, returned invisibly.
#'
#' @keywords internal
#' @noRd
choropleth_vm <- function (df, col = heat.colors(6), n = 6,
                           style = "quantile", distrib = TRUE, col_na = "grey",
                           pos_brks = TRUE, n_round = 0){

  # value
  value <- df@data %>%
    select_if(is.numeric) %>%
    unlist %>%  as.vector

  # choose class interval and colors
  classint <- suppressWarnings(classIntervals(value, n = n,
                                              style = style))
  pal <-  col[1:(length(classint$brks) - 1)]


  # replace the value of brks below zero by zero
  if (pos_brks == TRUE){
    classint$brks[classint$brks < 0] <- 0
  }

  # plot the result
  classint_colors <- findColours(classint, pal)
  classint_colors <- replace(classint_colors, is.na(classint_colors), col_na)

  # if ask, plot the quantile distribution (bottom right)
  if (distrib == TRUE){

    # graph parameter
    omar <- par("mar")
    m <- layout(matrix(c(2, 2, 2, 1), 2, 2, byrow = TRUE), heights=c(1, 2),
                widths = c(3, 1))
    # plots
    graphics::plot(classint, pal = pal, main = "")
    par(mar = c(2,2,2,2), bg = "transparent")
    sp::plot(df, col = classint_colors)
    par(mar = omar, bg = "white")

  } else {
    # graph parameters
    ofig <- par("fig")
    ousr <- par("usr")
    par(fig = ofig, usr = ousr)
    # plot
    sp::plot(df, col = classint_colors)
  }

  # Print the legend if needed :
  legend <- classint$brks %>% round(n_round)
  attr(legend, "colors") <- pal
  invisible(legend)

}

################################################################################
#' Draws a choropleth map with fixed breaks
#'
#' @param df a data frame containing two columns : one containing the province
#' name and another containing the value to represent
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param fixedBreaks issued from the \code{classint} package. By default
#' \code{NULL} but if a vector value is inputed, it will be used to specifen the
#'  breaks
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value, returned invisibly.
#'
#' @keywords internal
#' @noRd
choropleth_fix <- function (df, col = heat.colors(6), col_na = "grey",
                            fixedBreaks = NULL){

  # value
  value <- df@data %>%
    select_if(is.numeric) %>%
    unlist %>%  as.vector

  # choose class interval and colors
  pal <-  col[1:(length(fixedBreaks) - 1)]

  pal2 <- colorRampPalette(pal)
  df$col <- pal2(length(fixedBreaks) - 1)[cut(value, breaks = fixedBreaks,
                                              include.lowest = TRUE)]
  df$col <- replace(df$col, is.na(df$col), col_na)

  #return(provinces)
  sp::plot(df, col = df$col)

  # print the breaks for a legend
  legend <- fixedBreaks
  attr(legend, "colors") <- pal
  invisible(legend)
}

################################################################################
#' Draws a choropleth map
#'
#' @details The \code{n} parameter can be overuled by the number of breaks
#' obtain by certain style. By default, the number of intervall will be decide
#' by the \code{n} parameter but for certain \code{style} like "pretty", this
#' value can be overruled. Please for more information, look at the
#' documentation of the classint package. \cr
#' \cr
#' If the \code{fixedBreaks} parameter is filled, the \code{n} and \code{style}
#' parameters will be automatically overuled as \code{style = "fixed"} and
#' \code{n} will be equal to the length of the fixedBreaks vector - 1.
#'
#' @param df a data frame containing at two colums, one of class "character" and
#' one of class "numeric".
#' @param map an object of class "SpatialPolygonsDataFrame" containing at least
#' the varible \code{province}
#' @param n a numeric indicating the number of intervals to represent the data
#' (by default, \code{n = 6})
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param style a character value issued from the \code{classint} package, and
#' used to select a method for the different way of calculating the intervals
#' (by default \code{style = "quantile"})
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param fixedBreaks By default \code{NULL} but if a vector of value is
#' inputed, it will be used to specifen the breaks.
#' @param distrib if TRUE, print on the map, the distribution of the values by
#' intervals. Only available when the represented variable has at least two
#' differents value and if the breaks are not specified via fixedBreaks.
#' @param n_round integer indicating the number of significant digits to be used
#' @param pos_brks if TRUE, the breaks values will all be positive, the first
#' break will be superior or equal to zero, by default (\code{TRUE}). If false,
#' allows negative value for breaks
#'
#' @return A numeric vector with attributes corresponding of the breaks value
#' and the attributes \code{colors} corresponding to the color associated with
#' the breaks value, returned invisibly.
#'
#' @examples
#' library(poseid)
#' library(gdpm)
#' library(magrittr)
#' library(gadmVN)
#'
#' # dengue data
#' dengue <- getid(dengue, from = 1990, to = 2010)
#' # geographic data
#' map <- gadmVN::gadm(date = 1990, merge_hanoi = TRUE)
#'
#' # A choroplet map of the dengue data:
#' # The first step is to select only the month and the year that we want to
#' # represent, here the incidence value of dengue in September 1993. We keep
#' # only the 'province' and 'incidence_dengue' column as the 'choromap'
#' # function accept only dataframe of the two columns in entry.
#' # The second step is to print the map.
#' # The last step is to print a legend, here we use legend2 of the 'poseid'
#' # package but other function can be used.
#' library(dplyr)
#' dengue_0993  <- filter(dengue, year == 1993, month == "September")
#' dengue_0993 <- select(dengue_0993, province, contains("incidence"))
#' a <- choromap(dengue_0993, map)
#' # return invisibly the information for the legend
#' legend2(legend = a, col = attr(a, "colors"), col_na = "grey")
#'
#' # You can also use the '%>%' operator:
#' dengue_0993 %>%
#'   choromap(map) %>%
#'   legend2(legend = ., col = attr(., "colors"), col_na = "grey")
#'
#' # A choroplet map of the dengue data with some data transformations in order
#' # to reflect better the contrasts:
#' dengue_0993 %>%
#'  choromap(map, n = 9, style = "jenks", col = heat.colors(9)) %>%
#'  legend2(legend = ., col = attr(., "colors"), col_na = "grey")
#'
#' # Using some other color palettes, for examples the ones from the
#' # RColorBrewer package or the one in the color Palettes of R :
#' library(RColorBrewer)
#' # to see the available color palettes:
#' display.brewer.all()
#' dengue_0993 %>%
#'  choromap(map, n = 9, style = "jenks",
#'       col = brewer.pal(9, "YlOrRd")) %>%
#'  legend2(legend = ., col = attr(., "colors"), col_na = "grey")
#'
#' # changing the color of the missing values:
#' dengue_0993 %>%
#'  choromap(map, n = 9, style = "jenks",
#'       col = brewer.pal(9, "YlOrRd"), col_na = "blue") %>%
#'  legend2(legend = ., col = attr(., "colors"), col_na = "blue")
#'
#'
#' # Print the distribution of the value by intervals:
#' dengue_0993 %>%
#'  choromap(map, n = 9, style = "jenks",
#'       col = brewer.pal(9, "YlOrRd"), col_na = "blue",
#'       distrib = TRUE) %>%
#'  legend2(legend = ., col = attr(., "colors"), col_na = "blue")
#'
#' # Use the fixedBreaks parameters to define your own intervals of value:
#' dengue_0993 %>%
#'  choromap(map, fixedBreaks = c(0, 20, 100, 200, 400, 800, 1200, 1500),
#'       col = brewer.pal(7, "YlOrRd"), col_na = "blue") %>%
#'  legend2(legend = ., col = attr(., "colors"), col_na = "blue")
#'
#' @export
choromap <- function(df, map,
                 n = 6, col = heat.colors(6), style = "quantile",
                 col_na = "grey", pos_brks = TRUE, fixedBreaks = NULL,
                 distrib = FALSE, n_round = 0) {

  # graph parameters
  ofig <- par("fig")
  omar <- par("mar")
  par <- par(mar = c(2,2,2,2))
  on.exit(par(fig = ofig, mar = omar))

  # test entry
  # number of colums
  if (ncol(df) != 2){
    stop ("Invalid number of column, 'df' should only have two columns")
  }
  # class
  if (is.character(df[,1]) == FALSE & is.character(df[,2]) == FALSE){
    stop("Invalid 'df', one of the column needs to be of class 'character' and
         the other of class 'numeric'")
  }
  if (is.numeric(df[,1]) == FALSE & is.numeric(df[,2]) == FALSE){
    stop("Invalid 'df', one of the column needs to be of class 'character' and
         the other of class 'numeric'")
  }
  if (class(map) != "SpatialPolygonsDataFrame"){
    stop ("Invalid 'map' format, should be 'SpatialPolygonsDataFrame'")
  }
  # style input
  style_entry <- c("fixed", "sd", "equal", "pretty", "quantile", "kmeans",
                   "hclust", "bclust", "fisher", "jenks")
  if(!is.element(style, style_entry)){
    stop("The parameters 'style' can only contain: one of 'fixed', 'sd',
         'equal', 'pretty', 'quantile', 'kmeans', 'hclust', 'blust', 'fisher' or 'jenks'.
         For more information, please look at the package 'classInt'.")
  }

  # implement the data in the shape file data
  provinces <- sp::merge(map, df)

  # value
  value <- provinces@data %>%
    select_if(is.numeric) %>%
    unlist %>%  as.vector

  # draw a choropleth map when all the data contain one single data and no fixed
  # breaks
  if(length(na.omit(unique(value))) <= 1 &
     length(fixedBreaks) == 0)
  {
    #distrib <- NULL
    choropleth_v1(provinces, col = col, col_na = col_na, n_round = n_round)

  }
  # draw a choropleth with multiple values and no fixed breaks
  else if (length(na.omit(unique(value))) > 1 &
           length(fixedBreaks) == 0)
  {
    choropleth_vm(provinces, col = col, col_na = col_na, n = n, style = style,
                  distrib = distrib, pos_brks = pos_brks, n_round = n_round)
  }
  # draw a choropleth map with a fixed breaks
  else if (length(fixedBreaks) > 0){
    choropleth_fix(provinces, col = col, col_na = col_na,
                   fixedBreaks = fixedBreaks)
  }

}
