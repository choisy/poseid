library(gdpm) # for 'getid'
library(gadmVN) # for  'gadm'
library(dplyr) # for ' %>% '
library(magrittr) # for 'select', 'filter', 'mutate'

context("Test if `sthm` returns error when the input are not in a correct
        format")

test_that("`sthm` returns the correct error message", {

  cholera <- getid(cholera) %>%
   dplyr::mutate(time = as.Date(
     paste0(year, "-", as.numeric(month), "-", 15))) %>%
   dplyr::select(contains("province"), contains("time"),
                 contains("incidence")) %>%
   dplyr::arrange(time)
  col <- heat.colors(9)


expect_error(
  cholera %>%
    cbind(cholera$time) %>%
    sthm(f = function(x) x^.1, col = col) %>%
    legend2(.925, 1, legend =  ., col = col, postext = "right", n_round = 2,
            h = 1/length(col), w = 0.04, tl = 0.01, s = 0.005),
    "Invalid number of column, 'df' should only have three columns")


expect_error(
  cholera %>%
    dplyr::mutate(time = as.character(time)) %>%
    sthm(f = function(x) x^.1, col = col) %>%
    legend2(.925, 1, legend =  ., col = col, postext = "right", n_round = 2,
            h = 1/length(col), w = 0.04, tl = 0.01, s = 0.005),
    "Invalid 'df', one of the column needs to be of class 'character', one
         of class 'Date' and the last of class 'numeric'")


expect_error(
  cholera %>%
    dplyr::mutate(incidence_cholera = as.character(incidence_cholera)) %>%
    sthm(f = function(x) x^.1, col = col) %>%
    legend2(.925, 1, legend =  ., col = col, postext = "right", n_round = 2,
            h = 1/length(col), w = 0.04, tl = 0.01, s = 0.005),
    "Invalid 'df', one of the column needs to be of class 'character', one
         of class 'Date' and the last of class 'numeric'")


expect_error(
  cholera %>%
    dplyr::mutate(province = 1) %>%
    sthm(f = function(x) x^.1, col = col) %>%
    legend2(.925, 1, legend =  ., col = col, postext = "right", n_round = 2,
            h = 1/length(col), w = 0.04, tl = 0.01, s = 0.005),
    "Invalid 'df', one of the column needs to be of class 'character', one
         of class 'Date' and the last of class 'numeric'")

})


test_that("`sthm` returns the province in the good order", {

  dengue <- getid(dengue) %>%
    dplyr::mutate(time = as.Date(
      paste0(year, "-", as.numeric(month), "-", 15))) %>%
    dplyr::select(contains("province"), contains("time"),
                  contains("incidence")) %>%
    dplyr::arrange(time)

  provinces <- gadm(date = 1980)
  coord <- sp::coordinates(provinces)
  row.names(coord) <- unique(provinces@data$province)
  order <- rownames(coord[order(coord[, 2]), ])
  order <- data.frame(province = order, order = seq_along(order))
  dengue <- suppressWarnings(left_join(dengue, order, by = "province"))
  dengue <- dplyr::arrange(dengue, order) %>%
    dplyr::select(-order)

expect_identical(
  dengue %>%
    sthm(f = function(x) x^.1, col = heat.colors(9), show_legend = TRUE) %>%
      .[[2]],
  order$province %>%
    as.character() %>%
    grep("Ha Son Binh", .,value = T, invert = T))

})

