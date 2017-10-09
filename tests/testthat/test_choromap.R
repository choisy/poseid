library(gdpm) # for 'getid'
library(gadmVN) # for  'gadm'
library(dplyr) # for ' %>% '
library(magrittr) # for 'select', 'filter', 'mutate'

context("`choromap` returns error when the input are not in a correct
        format")

test_that("`choromap` returns the correct error message", {

dengue <- getid(dengue, from = 1992)
col <- rev(heat.colors(9))
map <- map <- gadmVN::gadm(date = 1992, merge_hanoi = TRUE)
map[which(map$province == "Ha Son Binh"),] <- "Ha Noi"

expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    dplyr::select(province, contains("incidence")) %>%
    mutate(province = incidence_dengue) %>%
    choromap(map = map, fixedBreaks = c(0,10,50,100,500,1000,2000)) %>%
    legend2(legend = ., col = attr(., "colors")),
  "Invalid 'df', one of the column needs to be of class 'character' and
         the other of class 'numeric'")

expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    choromap(map = map, fixedBreaks = c(0,10,50,100,500,1000,2000)) %>%
    legend2(legend = ., col = attr(., "colors")),
  "Invalid number of column, 'df' should only have two columns")

expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    dplyr::select(province, contains("incidence")) %>%
    choromap(map = dengue, fixedBreaks = c(0,10,50,100,500,1000,2000)) %>%
    legend2(legend = ., col = attr(., "colors")),
  "Invalid 'map' format, should be 'SpatialPolygonsDataFrame'")

expect_error(
  dengue  %>%
    dplyr::filter(year == 2004, month == "April") %>%
    dplyr::select(province, contains("incidence")) %>%
    choromap(map = map, fixedBreaks = c(0,10,50,100,500,1000,2000),
             col = heat.colors(3)) %>%
    legend2(legend = ., col = attr(., "colors")), regexp = NULL)

})
