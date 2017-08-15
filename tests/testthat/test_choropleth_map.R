library(gdpm) # for 'getid'
library(gadmVN) # for  'gadm'
library(dplyr) # for ' %>% '
library(magrittr) # for 'select', 'filter', 'mutate'

context("`choropleth_map` returns error when the input are not in a correct
        format")

test_that("`choropleth_map` returns the correct error message", {

dengue <- getid(dengue, from = 1992)
col <- rev(heat.colors(9))
map <- map <- gadmVN::gadm(date = 1992, merge_hanoi = TRUE)
map[which(map$province == "Ha Son Binh"),] <- "Ha Noi"


expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    dplyr::select(province, contains("incidence")) %>%
    mutate(province = incidence_dengue) %>%
    choropleth_map(map = map, n = 9, col = col, style = "fisher",
                   distrib = FALSE) %>%
    legend2(legend = ., col = attr(., "colors")),
  "Invalid 'df', one of the column needs to be of class 'character' and
         the other of class 'numeric'")

expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    choropleth_map(map = map, n = 9, col = col, style = "fisher",
                   distrib = FALSE) %>%
    legend2(legend = ., col = attr(., "colors")),
  "Invalid number of column, 'df' should only have two columns")

expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    dplyr::select(province, contains("incidence")) %>%
    choropleth_map(map = dengue, n = 9, col = col, style = "fisher",
                   distrib = FALSE) %>%
    legend2(legend = ., col = attr(., "colors")),
  "Invalid 'map' format, should be 'SpatialPolygonsDataFrame'")

expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    dplyr::select(province, contains("incidence")) %>%
    choropleth_map(map = map, n = 9, col = col, style = "bla") %>%
    legend2(legend = ., col = attr(., "colors"), col_na = "grey"),
  "The parameters 'style' can only contain: one of 'fixed', 'sd',
         'equal', 'pretty', 'quantile', 'kmeans', 'hclust', 'blust', 'fisher' or 'jenks'.
         For more information, please look at the package 'classInt'")
})

