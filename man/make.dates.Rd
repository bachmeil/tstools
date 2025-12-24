\name{make.dates}
\alias{make.dates}
\title{Generate Numeric Dates for Time Series}
\description{
  Generates a vector of numeric dates corresponding to a specified time series range and frequency, with an optional lag.
}
\usage{
make.dates(start, end, frequency, lagged=0)
}
\arguments{
  \item{start}{The start date of the time series (e.g., c(2000, 1)).}
  \item{end}{The end date of the time series (e.g., c(2002, 12)).}
  \item{frequency}{The frequency of the time series (e.g., 4 for quarterly, 12 for monthly).}
  \item{lagged}{Optional: An integer representing the lag to apply to the dates. Defaults to 0.}
}
\value{
  A numeric vector of dates.
}
\examples{
  make.dates(start=c(2000, 1), end=c(2000, 3), frequency=12)
}
\keyword{utilities}
\keyword{time series}

