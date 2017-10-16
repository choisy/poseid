#' Draws a choropleth map with one unique value
#'
#' This function draws a choropleth map when all the provinces or regions have
#' the same value.
#'
#' @param df an object of class "SpatialPolygonsDataFrame" containing also
#' the value to represent
#' @param col_name character, name of the column containing the data represented
#' in the choromap
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param n_round integer indicating the number of significant digits to be used
#' @param ...  arguments to be passed to plot
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value, returned invisibly.
#'
#' @keywords internal
#' @noRd
choropleth_v1 <- function (df, col_name, col = heat.colors(1), col_na = "grey",
                           ...){

  # value
  value <- df@data %>%
    select_(col_name) %>%
    unlist %>%  as.vector

  # define the color and the class intervals
  pal <-  col
  pal <- pal[1]

  pal2 <-  rep(pal, length(value))
  pal2[is.na(value)] <- col_na
  classint <- na.omit(unique(value))

  # draw a choropleth map
  sp::plot(df, col = pal2, ...)

  # print a legend
  legend <- rep(classint,2)
  attr(legend, "colors") <- pal
  invisible(legend)
}

################################################################################
#' Draws a choropleth map with fixed breaks
#'
#' @param df a data frame containing two columns : one containing the province
#' name and another containing the value to represent
#' @param col_name character, name of the column containing the data represented
#' in the choromap
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param fixedBreaks issued from the \code{classint} package. By default
#' \code{NULL} but if a vector value is inputed, it will be used to specifen the
#'  breaks
#' @param ...  arguments to be passed to plot
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value, returned invisibly.
#'
#' @keywords internal
#' @noRd
choropleth_fix <- function (df, col_name, col = heat.colors(6), col_na = "grey",
                            fixedBreaks = NULL, ...){

  # value
  value <- df@data %>%
    select_(col_name) %>%
    unlist %>%  as.vector


  # choose class interval and colors
  pal <-  col[1:(length(fixedBreaks) - 1)]

  pal2 <- colorRampPalette(pal)
  df$col <- pal2(length(fixedBreaks) - 1)[cut(value, breaks = fixedBreaks,
                                              include.lowest = TRUE)]
  df$col <- replace(df$col, is.na(df$col), col_na)

  #return(provinces)
  sp::plot(df, col = df$col, ...)

  # print the breaks for a legend
  legend <- fixedBreaks
  attr(legend, "colors") <- pal
  invisible(legend)
}

################################################################################
#' Draws a choropleth map
#'
#' @param df a data frame containing at two colums, one of class "character" and
#' one of class "numeric".
#' @param map an object of class "SpatialPolygonsDataFrame" containing at least
#' one column containing the same observations and name as the character column
#' of the dataframe \code{df}
#' @param fixedBreaks a vector of value used to specife the class intervals.
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)})
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param ...  arguments to be passed to plot
#'
#' @return A numeric vector with attributes corresponding of the breaks value
#' and the attributes \code{colors} corresponding to the color associated with
#' the breaks value, returned invisibly.
#'
#' @details It's important that the parameters \code{df} and \code{map} has one
#' columns in commun, to be able to link them easily.
#'
#' @examples
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
#' a <- choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000))
#' # return invisibly the information for the legend
#' legend2(legend = a, col = attr(a, "colors"), col_na = "grey")
#'
#' # You can also use the '%>%' operator:
#' dengue_0993 %>%
#'   choromap(map, fixedBreaks = c(0,10,50,100,500,1000,2000)) %>%
#'   legend2(legend = ., col = attr(., "colors"), col_na = "grey")
#'
#' # Using some other color palettes, for examples the ones from the
#' # RColorBrewer package or the one in the color Palettes of R, be careful
#' # to have a color vector of length n-1 compare to the lenght of fixedBreakss:
#' library(RColorBrewer)
#' # to see the available color palettes:
#' display.brewer.all()
#' dengue_0993 %>%
#'  choromap(map, fixedBreaks = c(0,10,50,100,500,1000,2000),
#'            col = brewer.pal(6, "YlOrRd")) %>%
#'  legend2(legend = ., col = attr(., "colors"), col_na = "grey")
#'
#' # changing the color of the missing values:
#' dengue_0993 %>%
#'  choromap(map, fixedBreaks = c(0,10,50,100,500,1000,2000),
#'            col = brewer.pal(6, "YlOrRd"), col_na = "blue") %>%
#'  legend2(legend = ., col = attr(., "colors"), col_na = "blue")
#'
#' @export
choromap <- function(df, map, fixedBreaks, col = heat.colors(6),
                     col_na = "grey", ...) {

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
    stop("Invalid 'map' format, should be 'SpatialPolygonsDataFrame'")
  }
  # length
  if (length(fixedBreaks) < (length(col) + 1)) {
    col <- col[c(1:(length(fixedBreaks) - 1))]
  }
  if (length(fixedBreaks) > (length(col) + 1)) {
    stop("The length of 'fixedBreaks' should be equal to length of the parameter
            'col' + 1")
  }

  # implement the data in the shape file data
  provinces <- sp::merge(map, df, duplicateGeoms = TRUE)
  provinces@data %<>% dplyr::filter(duplicated(.) == FALSE)

  # value
  val_name <- df %>%
    select_if(is.numeric) %>%
    names()

  # draw a choropleth map when all the data contain one single data and no fixed
  # breaks
  if(length(unique(fixedBreaks)) == 1)
  {
    choropleth_v1(provinces, val_name, col = col, col_na = col_na, ...)

  } else
  {
    choropleth_fix(provinces, val_name, col = col, col_na = col_na,
                   fixedBreaks = fixedBreaks, ...)
  }
}
