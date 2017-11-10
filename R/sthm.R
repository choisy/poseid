#' Makes a spatio-temporal heatmap of a disease
#'
#' @param df A  data frame. Should contains three variables: one of class
#' "character", one of class "date", and one colums of 'numeric' class
#' containing the value to represent.
#' @param f a transforming function. By default the identity function.
#' @param col a vector of colors to use for the heatmap.
#' @param col_na the color with which to represent the missing values.
#' @param x a numeric values between 0 and 1. In proportion of the
#' figure's range, this number express the location of the right end of the
#' heatmap, and  can be used for the beginning of a legend point
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default \code{FALSE}.
#' @param map object of class Spatial (see \code{Details})
#' @param xm a numeric value between 0 and 1. In proportion of the
#' figure's range, this number express the location of the right end of the
#' \code{map}, print at the left of the heatmap.
#'
#' @details A map of the province can be print on the left of the heatmap with
#' the parameters \code{map} and \code{xm}. The map need to have the same
#' spatial definitions as the data frame (\code{df}), so that each line of the
#' heatmap can be link with the map.
#'
#' @keywords internal
#' @noRd
draw_heatmap <- function(df, f = function(x) x, col = heat.colors(12),
                         col_na = "grey", x = .85, show_legend = FALSE,
                         map = NULL, xm = .2)
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
  values <- as.matrix(as.data.frame(values))
  values_transf <- f(values)

  # Options and graphical parameters:
  opar <- par()
  owar <- getOption("warn")
  on.exit({par(opar);
    options(warn = owar)})
  plt <- par("plt")
  options(warn = -1)

  # Legend:
  labels <- levels(cut(seq(min(values, na.rm = TRUE),
                           max(values, na.rm = TRUE), le = 512),
                      length(col)))
  legend <- c(0, as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", labels)))

  # to print the heatmap with the map of vietnam
  if(is.null(map) == FALSE){

    # graphic parameters
    plt[1:2] <- c(0, xm)
    par(plt = plt)

    # plot map
    sp::plot(map, col = "lightgrey", setParUsrBB = TRUE,
             xlim = c(bbox(map)["x", "min"] - bbox(map)["x", "min"] * 0.02,
                      bbox(map)["x", "max"] * 1.02))
    usr <- par("usr")

    # add the line
    centroids <- sp::coordinates(map) %>% data.frame()
    centroids$province <- rownames(centroids)
    ordered <- data.frame(province = provinces_names,
                          order = seq(1,length(provinces_names))) %>%
      inner_join(centroids, by = "province") %>%
      arrange(order)
    X1 <- ordered$X1
    Y1 <- ordered$X2
    X2 <- usr[2]
    step <-  (usr[4] - usr[3]) / length(provinces_names)
    Y2 <- seq(from = usr[3] + step/2, to = usr[4] - step/2, by = step)
    segments(X1, Y1, X2, Y2, col = "grey")


    # graph parameter
    plt[1:2] <- c(xm, x)
    par(plt = plt, new = TRUE)

    # Heatmap:
    image(time_vec, seq_len(ncol(values_transf)), values_transf, ann = FALSE,
          yaxt = "n")
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col = col_na)
    image(time_vec, seq_len(ncol(values_transf)), values_transf, col = col,
          add = TRUE)
    box(bty = "o")


  } else {

    # Heatmap:
    plt[2] <- x
    par(plt = plt)
    image(time_vec, seq_len(ncol(values_transf)), values_transf, ann = FALSE)
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col = col_na)
    image(time_vec, seq_len(ncol(values_transf)), values_transf, col = col,
          add = TRUE)
    box(bty = "o")

  }

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
#' @param x a numeric values between 0 and 1. In proportion of the
#' figure's range, this number express the location of the right end of the
#' heatmap, and  can be used for the beginning of a legend point
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default \code{FALSE}.
#' @param map object of class Spatial (see \code{Details})
#' @param xm a numeric value between 0 and 1. In proportion of the
#' figure's range, this number express the location of the right end of the
#' \code{map}, print at the left of the heatmap.
#'
#' @return A numeric vector corresponding to the value of the legend, returned
#' invisibly. \cr
#' If \code{show_legend = TRUE}, return a list
#' containing a numeric vector called \code{legend} corresponding to the value
#' of the legend, and a character vector called \code{province} containing the
#' name of the province in the order of the figure. \cr
#' \cr
#' A map of the province can be print on the left of the heatmap with
#' the parameters \code{map} and \code{xm}. The map need to have the same
#' spatial definitions as the data frame (\code{df}), so that each line of the
#' heatmap can be link with the map.
#'
#' @examples
#' # Packages and parameters
#' library(gdpm)
#' library(dplyr)
#' library(magrittr)
#' par(xpd = TRUE)
#'
#'
#' # A heatmap of the ILI data:
#' ili <- getid(ili, from = 2004) %>%
#'   dplyr::mutate(time = as.Date(
#'                 paste0(year, "-", as.numeric(month), "-", 15))) %>%
#'   dplyr::select(province, time, contains("incidence")) %>%
#'   dplyr::arrange(time)
#' sthm(ili)
#'
#' # With a legend by using legend2 function:
#' col <- rev(heat.colors(10))
#' a <- sthm(ili, col = col)
#' legend2(0.925, 1, legend =  a, col = col, postext = "right",
#'         h = 1/(length(a)-1), w = 0.04, tl = 0.01, s = 0.005)
#'
#' # A large number of color can be used to
#' col <- rev(heat.colors(100))
#' a <- sthm(ili, col = col, col_na = "blue")
#' legend2(0.925, 1, legend =  a, col = col, postext = "right", col_na = "blue",
#'         h = 1/(length(a)-1), w = 0.04, tl = 0.01, s = 0.005)
#'
#' col <- rev(heat.colors(400))
#' a <- sthm(ili, col = col, col_na = "blue")
#' legend2(0.925, 1, legend =  a, col = col, postext = "right", col_na = "blue",
#'         h = 1/(length(a)-1), w = 0.04, tl = 0.01, s = 0.005)
#'
#' # with some data transformations in order to reflect better the contrasts:
#' a <- sthm(ili, f = sqrt, col = col)
#' legend2(0.925, 1, legend =  a, col = col, postext = "right",
#'         h = 1/(length(a)-1), w = 0.035, tl = 0.01, s = 0.005, cex = 0.8)
#'
#' a <- sthm(ili, f = function(x) x^.3, col = col)
#' legend2(0.925, 1, legend =  a, col = col, postext = "right",
#'         h = 1/(length(a)-1), w = 0.04, tl = 0.01, s = 0.005, cex = 0.8)
#'
#' # changing the color of the missing values:
#' dengue <- getid(dengue)  %>%
#'   mutate(time = as.Date(paste0(year, "-", as.numeric(month), "-", 15))) %>%
#'   select(province, time, contains("incidence")) %>%
#'   arrange(time)
#' col <- rev(heat.colors(400))
#'
#' a <- sthm(dengue, f = sqrt, col = col)
#' legend2(0.925, 1, legend =  a, col = col, postext = "right",
#'         col_na = "grey", h = 1/(length(a) - 1), w = 0.04, tl = 0.01,
#'           s = 0.005)
#'
#' a <- sthm(dengue, f = sqrt, col = col, col_na = "blue")
#' legend2(0.925, 1, legend =  a, col = col, postext = "right", n_round = 2,
#'         col_na = "blue", h = 1/(length(a) - 1), w = 0.04, tl = 0.01,
#'           s = 0.005)
#'
#' # to order the provinces by latitude:
#' library(gadmVN)
#' map <- gadmVN::gadm("1980-01-01", merge_hanoi = TRUE)
#' coord <- sp::coordinates(map) %>% as.data.frame()
#' coord$province <- unique(map@data$province)
#' coord <- coord[order(coord$V2),]
#' coord$order <- seq(1, dim(coord)[1], 1)
#' dengue_order <- left_join(dengue, coord, by = "province") %>%
#'       arrange(order) %>%
#'       select(-order, -V1, -V2)
#' a <- sthm(dengue_order, f = function(x) x^.3, col = col)
#' legend2(0.925, 1, legend =  a, col = col, postext = "right",
#'       h = 1/(length(a)-1), w = 0.04, tl = 0.01, s = 0.005)
#'
#'
#' # to print the map of the province in the order of the figure:
#' a <- sthm(dengue_order, f = function(x) x^.3, col = col, map = map)
#' legend2(0.925, 1, legend =  a, col = col, postext = "right",
#'       h = 1/(length(a)-1), w = 0.04, tl = 0.01, s = 0.005)
#'
#' # to print the map of the province and the legend in the order of the figure:
#' a <- sthm(dengue_order, f = function(x) x^.3, col = col, map = map,
#'       show_legend = TRUE)
#' legend2(0.925, 1, legend =  a$legend, col = col, postext = "right",
#'       h = 1/(length(col)), w = 0.04, tl = 0.01, s = 0.005)
#' a
#' # list containing the legend vector and the province vector which correspond
#' # to the name of the province in the order of the figure.
#'
#' @export
sthm <- function(df,
                 f = function(x) x, col = heat.colors(12),
                 col_na = "grey",  x = .85, show_legend = FALSE,
                 map = NULL, xm = .2)
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
               show_legend = show_legend, map = map, xm = xm)
}

