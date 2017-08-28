#' Makes a spatio-temporal heatmap of a disease
#'
#' @param df A  data frame. Should contains three variables: one of class
#' "character", one of class "date", and one colums of 'numeric' class
#' containing the value to represent.
#' @param f a transforming function. By default the identity function.
#' @param col a vector of colors to use for the heatmap.
#' @param col_na the color with which to represent the missing values.
#' @param x a numeric values between 0 and 1. In proportion of the
#' figure's range, these numbers express the location of the right end of the
#' heatmap, and  can be used for the beginning of a legend point
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default FALSE.
#'
#' @keywords internal
#' @noRd
draw_heatmap <- function(df,
                         f = function(x) x, col = heat.colors(12),
                         col_na = "grey", x = .85, show_legend = FALSE)
{

  # Graphic parameters
  warn_old <- unlist(options("warn"))
  options(warn = -1)
  on.exit(options(warn = warn_old))

  # Data preparation:
  time_vec <- select_if(df, function(col) class(col) == "Date") %>%
    unlist %>% as.vector %>%
    as.Date(origin = "1970-01-01") %>%
    unique
  provinces_names <- select_if(df,is.character) %>%
    unlist %>% as.vector %>%
    unique
  values <- sapply(provinces_names, function(x){
    sel <-  names(select_if(df, is.character))
    subset(df, df[, sel] == x) %>%
                     select_if(is.numeric)
    })
  values <- f(as.matrix(as.data.frame(values)))

  # Options and graphical parameters:
  opar <- par()
  owar <- getOption("warn")
  on.exit({par(opar);
    options(warn = owar)})
  plt <- par("plt")
  options(warn = -1)

  # The heatmap:
  plt[2] <- x[1]
  par(plt = plt)
  image(time_vec, seq_len(ncol(values)), values, ann = FALSE)
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = col_na)
  image(time_vec, seq_len(ncol(values)), values, col = col, add = TRUE)
  box(bty = "o")

  # The legend:
  labels <- levels(cut(seq(min(values, na.rm = TRUE), max(values, na.rm = TRUE),
                           le = 512), length(col)))
  legend <- c(0, as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labels)))

  # Print the legend if needed :
  if (show_legend) return(list(legend = legend, prov = provinces_names)) else
    invisible(legend)
}

################################################################################
#' Makes a spatio-temporal heatmap of a disease
#'
#' @param df A  data frame. Should contains three variables: one of class
#' "character", one of class "Date", and one colums of 'numeric' class
#' containing the value to represent.
#' @param f a transforming function. By default the identity function.
#' @param col a vector of colors to use for the heatmap.
#' @param col_na the color with which to represent the missing values.
#' @param x a numeric value between 0 and 1. In proportion of the
#' figure's range, these numbers express the location of the right end of the
#' heatmap, and the beginning and end of the color scale that stands on the
#' right of the heatmap.
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default FALSE.
#'
#' @return A numeric vector corresponding to the value of the legend, returned
#' invisibly. \cr
#' If \code{show_legend = TRUE}, return a list
#' containing a numeric vector called \code{legend} corresponding to the value
#' of the legend, and a character vector called \code{province} containing the
#' name of the province in the order of the figure.
#'
#' @examples
#' library(poseid)
#' library(gdpm)
#' library(dplyr)
#' library(magrittr)
#' # A heatmap of the ILI data:
#' ili <- getid(ili, from = 2004) %>%
#'   dplyr::mutate(time = as.Date(
#'     paste0(year, "-", as.numeric(month), "-", 15))) %>%
#'   dplyr:: select(matches("province"), matches("time"),
#'     contains("incidence")) %>%
#'   dplyr::arrange(time)
#' sthm(ili)
#'
#' # With a legend by using legend2 function:
#' col <- rev(heat.colors(9))
#' a <- sthm(ili, col = col)
#' legend2(.925, 1, legend =  a, col = col, postext = "right", n_round = 2,
#'         h = 1/length(col), w = 0.04, tl = 0.01, s = 0.005)
#'
#' # with some data transformations in order to reflect better the contrasts:
#' a <- sthm(ili, f = sqrt, col = col)
#' legend2(.925, 1, legend =  a, col = col, postext = "right", n_round = 1,
#'         h = 1/length(col), w = 0.035, tl = 0.01, s = 0.005, cex = 0.8)
#'
#' a <- sthm(ili, f = function(x) x^.3, col = col)
#' legend2(.925, 1, legend =  a, col = col, postext = "right", n_round = 2,
#'         h = 1/length(col), w = 0.04, tl = 0.01, s = 0.005, cex = 0.8)
#'
#' # using some other color palettes, for examples the ones fromt the
#' # RColorBrewer package:
#' library(RColorBrewer)
#'
#' a <- sthm(ili, f = function(x) x^.3, col = brewer.pal(11, "RdYlGn"))
#' legend2(.925, 1, legend =  a, col = brewer.pal(11, "RdYlGn"),
#'         postext = "right", n_round = 2,
#'         h = 1/length(brewer.pal(11, "RdYlGn")), w = 0.04, tl = 0.01,
#'         s = 0.005)
#'
#' a <- sthm(ili, f = function(x) x^.3, col = brewer.pal(11, "RdYlBu"))
#' legend2(.925, 1, legend =  a, col = brewer.pal(11, "RdYlBu"),
#'         postext = "right", n_round = 2, h = 1/(length(a) - 1), w = 0.04,
#'         tl = 0.01,  s = 0.005)
#'
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(11, "PRGn"))
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(9, "YlOrRd"))
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(9, "YlOrBr"))
#'
#' # changing the color of the missing values:
#' rubella <- getid(rubella)  %>%
#'   mutate(time = as.Date(paste0(year, "-", as.numeric(month), "-", 15))) %>%
#'   select(matches("province"), matches("time"), contains("incidence")) %>%
#'   arrange(time)
#'
#' a <- sthm(rubella, f = sqrt, col = col)
#' legend2(.925, 1, legend =  a, col = col, postext = "right", n_round = 2,
#'         col_na = "grey", h = 1/(length(a) - 1), w = 0.04, tl = 0.01,
#'           s = 0.005)
#'
#' a <- sthm(rubella, f = sqrt, col = brewer.pal(9, "YlOrRd"), col_na = "blue")
#' legend2(.925, 1, legend =  a, col = col, postext = "right", n_round = 2,
#'         col_na = "blue", h = 1/(length(a) - 1), w = 0.04, tl = 0.01,
#'           s = 0.005)
#'
#' # to order the provinces by latitude:
#' library(gadmVN)
#' library(dplyr)
#' provinces <- gadm()
#' coord <- sp::coordinates(provinces)
#' row.names(coord) <- unique(provinces@data$province)
#' order <- rownames(coord[order(coord[, 2]), ])
#' order <- data.frame(province = order, order = seq_along(order))
#' rubella_order <- left_join(rubella, order, by = "province")
#' rubella_order <- arrange(rubella_order, order)
#' rubella_order <- select(rubella_order, -order)
#' a <- sthm(rubella_order, f = sqrt, col = brewer.pal(9, "YlOrRd"),
#'          col_na = "blue")
#' legend2(.925, 1, legend =  a, col = col, postext = "right", n_round = 2,
#'         col_na = "blue", h = 1/length(col), w = 0.04, tl = 0.01,
#'           s = 0.005)
#'
#' # to print the province in the order of the figure:
#' a <- sthm(rubella_order, f = sqrt, col = brewer.pal(9, "YlOrRd"),
#'           col_na = "blue", show_legend = TRUE)
#' legend2(.925, 1, legend =  a$legend, col = col, postext = "right",
#'         n_round = 2, col_na = "blue", h = 1/length(col), w = 0.04, tl = 0.01,
#'           s = 0.005)
#'a
#'# list containing the legend vector and the province vector which correspond
#'# to the name of the province in the order of the figure.
#'
#' @export
sthm <- function(df,
                 f = function(x) x, col = heat.colors(12),
                 col_na = "grey", x = .85, show_legend = FALSE)
{
  # Tests input
  # number of colums
  if (ncol(df) != 3){
    stop ("Invalid number of column, 'df' should only have three columns")
  }
  # class
  if (is.character(df[,1]) == FALSE & is.character(df[,2]) == FALSE &
      is.character(df[,3]) == FALSE){
    stop("Invalid 'df', one of the column needs to be of class 'character', one
         of class 'Date' and the last of class 'numeric'")
  }
  if (class(df[,1]) != "Date" & class(df[,2]) != "Date" &
      class(df[,3])!= "Date"){
    stop("Invalid 'df', one of the column needs to be of class 'character', one
         of class 'Date' and the last of class 'numeric'")
  }
  if (is.numeric(df[,1]) == FALSE & is.numeric(df[,2]) == FALSE &
      is.numeric(df[,3]) == FALSE){
    stop("Invalid 'df', one of the column needs to be of class 'character', one
         of class 'Date' and the last of class 'numeric'")
  }

  draw_heatmap(df, f = f, col = col, col_na = col_na, x = x,
               show_legend = show_legend)
}

