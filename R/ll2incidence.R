#' Computing Incidence From Line Listing
#'
#' \code{ll2incidence} takes individual dates of infections and returns incidences
#' by a specified time step.
#'
#' The \code{time} variable of the output is defined at regular time step as
#' defined by the \code{unit} argument. In absence of cases between two steps,
#' a zero incidence is explicitly displayed.
#'
#' @param \code{x} either a vector or a 1-column data frame of dates of \code{"Date"}
#' or \code{"POSIXct"} class.
#'
#' @param \code{unit} a character string (one of \code{"day"}, \code{"week"} or
#' \code{"month"}) specifying the temporal aggregation wished for the incidence
#' calculation. Value set to "day" by default.
#'
#' @return \code{ll2incidence} returns a 2-variable data frame of incidences
#' values with \code{time} and \code{incidence} variables.
#'
#' @examples
#' # A vector of infection dates of class \code{"Date"}:
#' head(infection_date_v)
#' incidence_day_1 <- ll2incidence(infection_date_v)
#' incidence_week_1 <- ll2incidence(infection_date_v, "week")
#' incidence_year_1 <- ll2incidence(infection_date_v, "year")
#'
#' # A vector of infection dates of class \code{"POSIXct"}:
#' head(infection_date_v)
#' incidence_day_2 <- ll2incidence(infection_posixct_v)
#' incidence_week_2 <- ll2incidence(infection_posixct_v, "week")
#' incidence_year_2 <- ll2incidence(infection_posixct_v, "year")
#'
#' # A 1-column data frame of infection dates of class \code{"Date"}:
#' head(infection_date_v)
#' incidence_day_3 <- ll2incidence(infection_date_df)
#' incidence_week_3 <- ll2incidence(infection_date_df, "week")
#' incidence_year_3 <- ll2incidence(infection_date_df, "year")
#'
#' # A 1-column data frame of infection dates of class \code{"POSIXct"}:
#' head(infection_date_v)
#' incidence_day_4 <- ll2incidence(infection_posixct_df)
#' incidence_week_4 <- ll2incidence(infection_posixct_df, "week")
#' incidence_year_4 <- ll2incidence(infection_posixct_df, "year")
#'
#' # Comparing the results:
#' identical(incidence_day_1, incidence_day_2)
#' identical(incidence_day_1, incidence_day_3)
#' identical(incidence_day_1, incidence_day_4)
#' identical(incidence_day_2, incidence_day_3)
#' identical(incidence_day_2, incidence_day_4)
#' identical(incidence_day_3, incidence_day_4)
#' identical(incidence_week_1, incidence_week_2)
#' identical(incidence_week_1, incidence_week_3)
#' identical(incidence_week_1, incidence_week_4)
#' identical(incidence_week_2, incidence_week_3)
#' identical(incidence_week_2, incidence_week_4)
#' identical(incidence_week_3, incidence_week_4)
#' identical(incidence_year_1, incidence_year_2)
#' identical(incidence_year_1, incidence_year_3)
#' identical(incidence_year_1, incidence_year_4)
#' identical(incidence_year_2, incidence_year_3)
#' identical(incidence_year_2, incidence_year_4)
#' identical(incidence_year_3, incidence_year_4)
#'
#' # Looking at the results:
#' head(incidence_day_1)
#' head(incidence_week_1)
#' head(incidence_year_1)
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom lubridate round_date
#' @importFrom lubridate as_date
#' @export
#'
#' @author Marc Choisy
#'
ll2incidence <- function(x, unit = c("day", "week", "month")) {
#  require(magrittr)   # for the " %>% " and " %<>% " pipe operators
#  require(lubridate)  # for "round_date", "as_date"

  clnames <- c("Date", "POSIXct")
  mess_class <- "Dates in x should be of class Date or POSIXct"

  unit <- match.arg(unit)

# checking the format and class of data x:
  d <- dim(x)
  if(is.null(d)) {  # if x is a vector:
    if(!(class(x)[1] %in% clnames)) stop(mess_class)  # checking the class of the dates
  } else {          # if x is not a vector:
    if(d[2] > 1 | !any(class(x) %in% "data.frame")) { # make sure it is a 1-column data frame
      stop("x should be a vector or a 1-column data frame")
    } else {        # vectorize the data:
      cl <- sapply(x, class)[1, 1]
      if(!(cl %in% clnames)) stop(mess_class)         # checking the class of the dates
      fct <- setNames(c(as.Date, as.POSIXct), clnames)[[cl]]
      x %<>% mutate_all(as.character) %>% unlist %>% fct
    }
  }

# doing the transformations:
  x %>%
    round_date(unit) %>%
    table %>%
    data.frame %>%
    setNames(c("date", "incidence")) %>%
    mutate(date = as_date(date)) %>%
    right_join(data.frame(date = seq(min(.$date), max(.$date), unit)), by = "date") %>%
    mutate(incidence = ifelse(is.na(incidence), 0, incidence))
}

