#' poseid: Population-based Observational Studies of the Epidemiology of Infectious Diseases
#'
#' The poseid package provide functions of visualizing and analysing
#' population-based observational data in epidemiology.
#'
#' @importFrom dplyr select_if filter select_ inner_join
#' @importFrom sp merge bbox plot coordinates
#' @importFrom magrittr %>% %<>%
#' @importFrom classInt classIntervals findColours
#' @importFrom grDevices colorRampPalette heat.colors
#' @importFrom graphics box image layout locator par rect segments strwidth text
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @importFrom graphics plot
#' @importFrom stats end start
#'
#' @docType package
#' @name poseid
NULL

## quiets concerns of R CMD check for the values that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "incidence", "key",
                                                        "middle", "month",
                                                        "name", "province",
                                                        "value"))
