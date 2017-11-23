## ----include=F-----------------------------------------------------------
knitr::knit_hooks$set(margin = function(before,options,envir) {
if(before) par(mgp=c(1,0.35,0),bty="n",plt=c(0,.99,.13,.99), mar = c(2,2,3,2), xpd = TRUE) else NULL })

knitr::opts_chunk$set(margin=T,prompt=T,comment="",collapse=T,cache=F, bty="n",
dev.args=list(pointsize=11),fig.height= 4,
fig.width=6.24725,fig.retina=2,fig.align="center")

## ----eval=F--------------------------------------------------------------
#  devtools::install_github("choisy/gdpm")
#  devtolls::install_github("choisy/poseid")

## ------------------------------------------------------------------------
library(gdpm)
library(poseid)

## ------------------------------------------------------------------------
dengue <- getid(dengue, from = 2000)
head(dengue)

## ---- eval = FALSE-------------------------------------------------------
#  ?poseid::sthm

## ------------------------------------------------------------------------
str(dengue)

## ---- message=FALSE------------------------------------------------------
# the dengue dataframe need to be transform before visualization and we need the `dplyr` package for that.
#install.package("dplyr")
library(dplyr)

## ------------------------------------------------------------------------
# Need to have the time expressed in one column in a Data format
dengue <- mutate(dengue, 
                 time = as.Date(paste0(year, "-", as.numeric(month), "-", 15)))
dengue <- arrange(dengue, time)
dengue <- select(dengue, province, time, contains("incidence"))
# We can check now that dengue is in a good format.
str(dengue)

## ------------------------------------------------------------------------
sthm(dengue)

## ---- eval=FALSE---------------------------------------------------------
#  ?legend2

## ------------------------------------------------------------------------
a <- sthm(dengue)
# we use the parameter of sthm to complete the legend2 parameters, by default col = heat.colors(12), x = 0.85
legend2(.9, 1, legend =  a, col = heat.colors(12), postext = "right", h = 1/(length(a) - 1), w = 0.04, tl = 0.01, s = 0.005)

## ------------------------------------------------------------------------
col <- rev(heat.colors(200))

## ------------------------------------------------------------------------
a <- sthm(dengue, col = col)
# we use the parameter of sthm to complete the legend2 parameters
legend2(.9, 1, legend =  a, col = col, postext = "right", h = 1/(length(a) -1), w = 0.04, tl = 0.01, s = 0.005)

## ------------------------------------------------------------------------
a <- sthm(dengue, f = sqrt, col = col)
legend2(.9, 1, legend =  a, col = col, postext = "right", h = 1/(length(a) -1), w = 0.04, tl = 0.01, s = 0.005)

## ------------------------------------------------------------------------
a <- sthm(dengue, f = function(x) x^.3, col = col)
legend2(.9, 1, legend =  a, col = col, postext = "right", h = 1/(length(a) -1), w = 0.04, tl = 0.01, s = 0.005)

## ------------------------------------------------------------------------
dengue <- getid(dengue)
# data preparation
dengue <- mutate(dengue, time = as.Date(paste0(year, "-", as.numeric(month), "-", 15)))
dengue <- select(dengue, matches("province"), matches("time"), contains("incidence"))
dengue <- arrange(dengue, time)

str(dengue)

## ------------------------------------------------------------------------
a <- sthm(dengue, f = function(x) x^.3, col = col)
legend2(.9, 1, legend =  a, col =  col, postext = "right", col_na = "grey", size_na = 0.05, h = 1/(length(a) -1), w = 0.04, tl = 0.01, s = 0.005)

## ------------------------------------------------------------------------
a <- sthm(dengue, f = function(x) x^.3, col = col, col_na = "black")
legend2(.9, 1, legend =  a, col =  col, postext = "right", col_na = "black", size_na = 0.03, h = 1/(length(a) -1), w = 0.04, tl = 0.01, s = 0.005)

## ---- message = FALSE----------------------------------------------------
library(gadmVN)
library(sp)

## ------------------------------------------------------------------------
# extract the coordinates of each province
provinces <- gadmVN::gadm(date = "1980-01-01", merge_hanoi = TRUE)
coord <- sp::coordinates(provinces)
row.names(coord) <- unique(provinces@data$province)
head(coord)
# order the provinces by latitude and create an index called "order"
order <- rownames(coord[order(coord[, 2]), ])
order <- data.frame(province = order, order = seq_along(order))
head(order)

## ------------------------------------------------------------------------
dengue <- left_join(dengue, order, by = "province")
dengue <-  arrange(dengue, order)
head(dengue)

## ------------------------------------------------------------------------
dengue_or <- select(dengue, -order)
str(dengue_or)

## ------------------------------------------------------------------------
a <- sthm(dengue_or, f = function(x) x^.3, col = col, show_legend = TRUE)
# a is a list containing both the legend and the province. To print the scale legend with legend2, it is important to specify 'a$legend'
legend2(.9, 1, legend =  a$legend, col = col, postext = "right", h = 1/(length(a$legend) -1), w = 0.04, tl = 0.01, s = 0.005)
str(a)

## ------------------------------------------------------------------------
a <- sthm(dengue_or, f = function(x) x^.3, col = col, map = provinces, xm = 0.25, show_legend = TRUE)
legend2(.9, 1, legend =  a$legend, col = col, postext = "right", h = 1/(length(a$legend) -1), w = 0.04, tl = 0.01, s = 0.005)
str(a)

## ------------------------------------------------------------------------
#install.package("magrittr")
library(magrittr)

## ------------------------------------------------------------------------
# Geographic data
provinces <- gadmVN::gadm(date = "1980-01-01", merge_hanoi = TRUE)
coord <- sp::coordinates(provinces)
row.names(coord) <- unique(provinces@data$province)
# Order the provinces by latitude and create an index called "order"
order <- rownames(coord[order(coord[, 2]), ])
order <- data.frame(province = order, order = seq_along(order))

# Color vector
col <- rev(heat.colors(100))

# Pipe
getid(dengue) %>%
  mutate(time = as.Date(paste0(year, "-", as.numeric(month), "-", 15))) %>%
  select(province, time, contains("incidence")) %>% 
  left_join(order, by = "province") %>% 
  arrange(order) %>%
  select(-order) %>% 
  arrange(time) %>%
  sthm(f = function(x) x^.3, col = col, map = provinces, xm = 0.25) %>%
  legend2(.9, 1, legend =  ., col = col, postext = "right",
          h = 1/(length(.)-1), w = 0.04, tl = 0.01, s = 0.005)

## ---- eval = FALSE-------------------------------------------------------
#  ?choromap

## ------------------------------------------------------------------------
# Preparation data spatial and epidemiologic
# dengue data
dengue <- getid(dengue, from = 1993, to = 1993)
dengue_0993  <- filter(dengue, year == 1993, month == "September")
dengue_0993 <- select(dengue_0993, province, contains("incidence"))


## ------------------------------------------------------------------------
?poseid::breaks

## ---- eval = FALSE-------------------------------------------------------
#  ?classInt::classIntervals

## ------------------------------------------------------------------------
# plot Quantile
q_breaks <- poseid::breaks(dengue_0993, "incidence_dengue", pal = rev(heat.colors(6)), distribution = TRUE)
attr(q_breaks, "breaks")

## ------------------------------------------------------------------------
# plot Fisher - Jenkins
f_breaks <- poseid::breaks(dengue_0993, "incidence_dengue", style = "fisher" ,pal = rev(heat.colors(6)), distribution = TRUE)
attr(f_breaks, "breaks")

## ------------------------------------------------------------------------
# geographic data
map <- gadmVN::gadm(date = 1992)

## ------------------------------------------------------------------------
a <- choromap(dengue_0993, map, fixedBreaks = c(0, 75, 200, 490, 720, 1100, 1300), col = rev(heat.colors(6)), col_na = "grey")
# By default, the legend is on the top-left part of the figure, for more information: `?legend2`
legend2(legend = a, col = attr(a, "colors"), col_na = "grey")

## ------------------------------------------------------------------------
# Geographical information
map <- gadmVN::gadm(date = 1992)

# Pipe
getid(dengue, from = 1992, to = 1993) %>%
  filter(year == 1993, month == "September") %>%
  select(province, contains("incidence")) %>%
  breaks("incidence_dengue",n = 6, style = "fisher",
         pal = rev(heat.colors(6))) %>%
  choromap(., map, fixedBreaks = attr(., "breaks"),
           col = rev(heat.colors(6)), col_na = "gray") %>%
  legend2(legend = ., col = attr(., "colors"), col_na = "gray")

## ------------------------------------------------------------------------
getid(dengue, from = 1992, to = 1993) %>%
  filter(year == 1993, month == "September") %>%
  select(province, contains("incidence")) %>%
  breaks("incidence_dengue",n = 6, style = "fisher",
         pal = rev(heat.colors(6)), distribution = TRUE) %>%
  choromap(., map, fixedBreaks = attr(., "breaks"),
           col = rev(heat.colors(6)), col_na = "gray") %>%
  legend2(legend = ., col = attr(., "colors"), col_na = "gray")

