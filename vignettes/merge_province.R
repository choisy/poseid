## ----include=F-----------------------------------------------------------
knitr::knit_hooks$set(margin = function(before,options,envir) {
if(before) par(mgp=c(1,0.35,0),bty="n",plt=c(0,.99,.13,.99), mar = c(2,2,3,2), xpd = TRUE) else NULL })

knitr::opts_chunk$set(margin=T,prompt=T,comment="",collapse=T,cache=F, bty="n",
dev.args=list(pointsize=11),fig.height= 4,
fig.width=6.24725,fig.retina=2,fig.align="center")

## ----eval=F--------------------------------------------------------------
#  devtolls::install_github("choisy/poseid")
#  devtools::install_github("choisy/gso")

## ------------------------------------------------------------------------
library(poseid)
library(gso)

## ------------------------------------------------------------------------
sel <- grep("Crude birth rate, crude death rate and natural increase rate by province", 
            gso::data_frame_summary$`data frame`)
data_frame_summary$data_name[sel]
demo <- demography_8
head(demo)

## ------------------------------------------------------------------------
range(demo$year)
unique(demo$province)

## ------------------------------------------------------------------------
?merge_prov

## ---- message=FALSE------------------------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
demo <- filter(demo, is.element(province, 
                c("Ha Noi", "Hoa Binh", "Ha Tay", "Ha Son Binh", "Dak Lak", "Dak Nong")) == TRUE)
head(demo)
unique(demo$province)

## ------------------------------------------------------------------------
mdemo <- merge_prov(demo, from = "2000-01-01")
head(mdemo)
unique(mdemo$province)

## ------------------------------------------------------------------------
mdemo <- merge_prov(demo, from = 1990, to = 2010)
head(mdemo)
unique(mdemo$province)
range(mdemo$year)

## ------------------------------------------------------------------------
merge_prov(demo, from = 1990, to = 2010, FUN = mean)

## ------------------------------------------------------------------------
pop_size <- gso::pop_size
pop_size <- mutate_if(pop_size, !is.element(names(pop_size), c("province", "year")), funs(1000 * .))
head(pop_size)

## ------------------------------------------------------------------------
merge_prov(demo, from = 1990, FUN = weighted.mean, df2 = pop_size, args = "total")

## ------------------------------------------------------------------------
merge_prov(demo, sel = "crude_birth_rate", from = 1990, FUN = weighted.mean, df2 = pop_size, args = "total") 

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("choisy/gdpm", build_vignettes = TRUE) # to install the gdpm package
#  vignette(package = "gdpm")

## ------------------------------------------------------------------------
library(gdpm)
?getid

## ------------------------------------------------------------------------
hepatitis <- getid(hepatitis, from = 1990, to = 2017)

## ------------------------------------------------------------------------
demo <- demography_8
mdemo <- merge_prov(demo, sel = "crude_birth_rate", from = 1990, FUN = weighted.mean,
                    df2 = pop_size, args = "total") 
unique(mdemo$province)
identical(unique(hepatitis$province), unique(mdemo$province))

## ------------------------------------------------------------------------
mdemo <- merge_prov(demo, sel = "crude_birth_rate", from = 1990, FUN = weighted.mean,
                    df2 = pop_size, args = "total", diseases = "hepatitis") 
unique(mdemo$province)
identical(unique(hepatitis$province), unique(mdemo$province))

