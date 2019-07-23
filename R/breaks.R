#' Selects the breaks
#'
#' This function selects the class intervals for a numeric column in a data
#' frame.
#'
#' @param df a data frame containing at least one numeric column
#' @param col_name name of the column containing the value on which the class
#' intervals should be calculate
#' @param n a numeric indicating the number of intervals to represent the data
#' (by default, \code{n = 6})
#' @param style a character value issued from the \code{classint} package, and
#' used to select a method for the different way of calculating the intervals
#' (by default \code{style = "quantile"})
#' @param pal the color for the distribution legend, by default \code{NULL}
#' @param fixedBreaks By default \code{NULL} but if a vector of value is
#' inputted, it will be used to specifen the breaks, need to used with the
#' parameter \code{style = "fixed"}
#' @param distribution if TRUE, print on the map, the distribution of the values
#' by intervals
#'
#' @details For more information on the parameter \code{style}, please, look at
#' the package \code{classint}.
#'
#' @return The data frame inputed with attributes \code{breaks} corresponding to
#'  the breaks value, returned invisibly.
#'
#' @examples
#'
#' # Example with data preparation
#' library(gdpm)
#' library(dplyr)
#' dengue <- getid(dengue, from = 1990, to = 1991)
#' dengue_prep <- filter(dengue, year == 1990, month == "September")
#'
#' a <- breaks(dengue_prep, "incidence_dengue")
#' str(a)
#' # to extract the breaks value
#' attr(a, "breaks")
#'
#' # to print the distribution
#' breaks(dengue_prep, "incidence_dengue", pal = heat.colors(6),
#'        distribution = TRUE)
#'
#' # to change the style or number of breaks
#' breaks(dengue_prep, "incidence_dengue", n = 8, style = "fisher")
#' breaks(dengue_prep, "incidence_dengue", n = 8, style = "fisher",
#'        pal = heat.colors(8), distribution = TRUE)
#'
#' # to use the "fixed" style
#' breaks(dengue_prep, "incidence_dengue", n = 6, style = "fixed",
#'        fixedBreaks = c(0, 50, 100, 500, 1000, 1500, 2000))
#' breaks(dengue_prep, "incidence_dengue", n = 6, style = "fixed",
#'        pal = heat.colors(6), distribution = TRUE,
#'        fixedBreaks = c(0, 50, 100, 500, 1000, 1500, 2000))
#'
#' @export
breaks <- function(df, col_name, n = 6, style = "quantile", pal = NULL,
                   fixedBreaks = NULL, distribution = FALSE){

  # style input test
  style_entry <- c("fixed", "sd", "equal", "pretty", "quantile", "kmeans",
                   "hclust", "bclust", "fisher", "jenks")
  if (!is.element(style, style_entry)) {
    stop("The parameter 'style' can only contain: one of 'fixed', 'sd',
         'equal', 'pretty', 'quantile', 'kmeans', 'hclust', 'blust', 'fisher' or
         'jenks'. For more information, please look at the package 'classInt'.")
  }

  if (is.null(fixedBreaks) == FALSE & length(fixedBreaks) != n + 1) {
    stop("The parameter 'fixedBreaks' should be of length 'n' + 1 ")
  }

  if (is.null(fixedBreaks) == FALSE & is.null(pal) == FALSE &
     length(fixedBreaks) != length(pal) + 1) {
    stop("The parameter 'fixedBreaks' should be equal to the length of
         'pal' + 1 ")
  }

  value <- df %>%
    select_if(is.numeric) %>%
    select_(col_name) %>%
    unlist %>%  as.vector

  # for selection of breaks of data containing one unique value
  if (length(na.omit(unique(value))) <= 1) {
    breaks <- c(0, max(value))
  } else {
    # select breaks
    breaks <- suppressWarnings(classIntervals(value, n = n, style = style,
                                              fixedBreaks = fixedBreaks))

    # distribution (if parameters TRUE)
    if (distribution == TRUE){
      # plots
      plot(breaks, pal = pal, main = "distribution", ann = FALSE)
    }
    breaks <- breaks$brks
  }
  attr(df, "breaks") <- breaks
  invisible(df)
}
