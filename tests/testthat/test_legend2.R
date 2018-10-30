library(gdpm) # for 'getid'
library(magrittr) # for ' %>% '
library(gadmVN) # for 'gadm'

context("Test if `legend2` returns error when the input are not in a correct
        format")

test_that("`legend2` returns the correct error message", {

dengue <- getid(dengue, from = 1992, to = 2010)
map <- gadmVN::gadm(date = 1992, merge_hanoi = TRUE) %>% sf::as_Spatial()

dengue_0993  <- dplyr::filter(dengue, year == 1993, month == "September") %>%
  dplyr::select(province, contains("incidence"))

expect_error(
  choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000)) %>%
  legend2(legend = ., col = attr(., "colors"), postext = "blue"),
  "The parameters 'postext' can only contain: 'left' or 'right'")

expect_error(
  choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000)) %>%
  legend2(legend = ., col = attr(., "colors"), locate = 1),
  "The parameters 'locate' can only be a logical: 'TRUE' or 'FALSE'")

expect_error(
  choromap(dengue_0993, map, fixedBreaks = c(0,10,50,100,500,1000,2000)) %>%
  legend2(legend = ., col = attr(., "colors"), pos = "blue"),
  "The parameters 'pos' can only contain: 'top-left', 'top-right',
           'bottom-left' or 'bottom-right'")

})
