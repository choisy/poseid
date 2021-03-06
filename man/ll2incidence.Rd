% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ll2incidence.R
\name{ll2incidence}
\alias{ll2incidence}
\title{Computing Incidence From Line Listing}
\usage{
ll2incidence(x, unit = c("day", "week", "month", "quarter", "year"))
}
\arguments{
\item{x}{either a vector or a 1-column data frame of dates of \code{"Date"}
or \code{"POSIXct"} class.}

\item{unit}{a character string (one of \code{"day"}, \code{"week"},
\code{"month"}, \code{"quarter"} or \code{"year"}) specifying the temporal
aggregation wished for the incidence calculation. Value set to "day" by
default.}
}
\value{
\code{ll2incidence} returns a data frame of 4 variables:
\code{start}, \code{middle}, \code{end} and \code{incidence}. The first 3
variables are respectively the dates of the beginning, the middle and the end
of the time period over which the incidence is defined and are of class
\code{POSIXct} with time zone UTC. The HH:MM:SS of the \code{start} and
\code{end} variables are 00:00:00.
}
\description{
\code{ll2incidence} takes individual dates of infections and returns
population incidences by a specified time step.
}
\details{
The \code{date} variable of the output is defined with a regular time step as
defined by the \code{unit} argument. In absence of cases between two steps, a
zero incidence is explicitly displayed.
}
\examples{
# Four different data sets of infections dates:
infection_date_v <- infections_dates
infection_posixct_v <- as.POSIXct(infection_date_v)
infection_date_df <- as.data.frame(infection_date_v)
infection_posixct_df <- as.data.frame(infection_posixct_v)
data_sets <- list(infection_date_v,      # vector of Date class
                  infection_date_df,     # data frame of Date class
                  infection_posixct_v,   # vector of POSIXct class
                  infection_posixct_df)  # data frame of POSIXct class
lapply(data_sets, head)

# Five time resolutions we want to consider for incidence calculations:
steps <- c("day", "week", "month", "quarter", "year")

# Calculating of the 4 x 5 = 20 incidence data sets:
incidences <- lapply(steps,
                     function(y) lapply(data_sets, function(x)
                      ll2incidence(x, y)))

# Comparing the results:
n <- length(incidences[[1]])
any(!unlist(lapply(1:n,
                   function(x) sapply(1:(n - 1),
                     function(y) sapply((y + 1):n, function(z)
                      identical(incidences[[c(x, y)]],
                       incidences[[c(x, z)]]))))))

# Showing the results:
for(i in 1:5) print(head(incidences[[c(i, 1)]]))

# Checking that the number of cases is the same whatever the data type:
any(!as.vector(sapply(1:4,
        function(x) sapply(1:5,
          function(y) sum(incidences[[c(y, x)]]$incidence)))) ==
           length(infections_dates))

}
\author{
Marc Choisy
}
