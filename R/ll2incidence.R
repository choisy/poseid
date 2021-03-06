#' Computing Incidence From Line Listing
#'
#' \code{ll2incidence} takes individual dates of infections and returns
#' population incidences by a specified time step.
#'
#' The \code{date} variable of the output is defined with a regular time step as
#' defined by the \code{unit} argument. In absence of cases between two steps, a
#' zero incidence is explicitly displayed.
#'
#' @param x either a vector or a 1-column data frame of dates of \code{"Date"}
#' or \code{"POSIXct"} class.
#' @param unit a character string (one of \code{"day"}, \code{"week"},
#' \code{"month"}, \code{"quarter"} or \code{"year"}) specifying the temporal
#' aggregation wished for the incidence calculation. Value set to "day" by
#' default.
#'
#' @return \code{ll2incidence} returns a data frame of 4 variables:
#' \code{start}, \code{middle}, \code{end} and \code{incidence}. The first 3
#' variables are respectively the dates of the beginning, the middle and the end
#' of the time period over which the incidence is defined and are of class
#' \code{POSIXct} with time zone UTC. The HH:MM:SS of the \code{start} and
#' \code{end} variables are 00:00:00.
#'
#' @author Marc Choisy
#'
#' @examples
#' # Four different data sets of infections dates:
#' infection_date_v <- infections_dates
#' infection_posixct_v <- as.POSIXct(infection_date_v)
#' infection_date_df <- as.data.frame(infection_date_v)
#' infection_posixct_df <- as.data.frame(infection_posixct_v)
#' data_sets <- list(infection_date_v,      # vector of Date class
#'                   infection_date_df,     # data frame of Date class
#'                   infection_posixct_v,   # vector of POSIXct class
#'                   infection_posixct_df)  # data frame of POSIXct class
#' lapply(data_sets, head)
#'
#' # Five time resolutions we want to consider for incidence calculations:
#' steps <- c("day", "week", "month", "quarter", "year")
#'
#' # Calculating of the 4 x 5 = 20 incidence data sets:
#' incidences <- lapply(steps,
#'                      function(y) lapply(data_sets, function(x)
#'                       ll2incidence(x, y)))
#'
#' # Comparing the results:
#' n <- length(incidences[[1]])
#' any(!unlist(lapply(1:n,
#'                    function(x) sapply(1:(n - 1),
#'                      function(y) sapply((y + 1):n, function(z)
#'                       identical(incidences[[c(x, y)]],
#'                        incidences[[c(x, z)]]))))))
#'
#' # Showing the results:
#' for(i in 1:5) print(head(incidences[[c(i, 1)]]))
#'
#' # Checking that the number of cases is the same whatever the data type:
#' any(!as.vector(sapply(1:4,
#'         function(x) sapply(1:5,
#'           function(y) sum(incidences[[c(y, x)]]$incidence)))) ==
#'            length(infections_dates))
#'
#' @importFrom lubridate as_datetime floor_date ceiling_date
#' @export
#'
ll2incidence <- function(x, unit =
                           c("day", "week", "month", "quarter", "year")) {

  clnames <- c("Date", "POSIXct")
  mess_class <- "Dates in x should be of class Date or POSIXct"

  unit <- match.arg(unit)

# checking the format and class of data x:
  d <- dim(x)
  if (is.null(d)) {
    # if x is a vector: checking the class of the dates
    if (!(class(x)[1] %in% clnames)) stop(mess_class)
  } else {
    # if x is not a vector: make sure it is a 1-column data frame
    if (d[2] > 1 | !any(class(x) %in% "data.frame")) {
      stop("x should be a vector or a 1-column data frame")
    } else {
      cl <- vapply(x, class, rep("a", length(class(x[[1]])))) # vectorize
      cl <- as.vector(cl)[1]
      # checking the class of the dates
      if (!(any(cl %in% clnames))) stop(mess_class)
      fct <- setNames(c(as.Date, as.POSIXct), clnames)[[cl]]
      x <- fct(as.character(x[, 1]))
    }
  }

# doing the transformations:
  res <- table(lubridate::floor_date(x, unit))
  res <- data.frame(res, stringsAsFactors = FALSE)
  res <- setNames(res, c("start", "incidence"))
  res <- transform(res, start = lubridate::as_datetime(res$start))
  res <- merge(data.frame(start = seq(min(res$start), max(res$start), unit)),
               res, by = "start", all = "TRUE")
  res <- transform(res,
                   incidence =
                     as.integer(ifelse(is.na(res$incidence), 0, res$incidence)),
                   end = ceiling_date(res$start + 1, unit))
  res <- transform(res,
                   middle = ceiling_date(res$start + (res$end - res$start) / 2))
  res <- res[, c("start", "middle", "end", "incidence")]
}
