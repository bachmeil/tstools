\name{dates}
\alias{dates}
\title{Generate Numeric Time Series Dates}
\description{
  Generates a sequence of numeric dates for a time series given start, end, and frequency.
}
\usage{
dates(start, end, freq)
}
\arguments{
  \item{start}{The start date of the time series (e.g., c(1990, 1) or 1990).}
  \item{end}{The end date of the time series (e.g., c(2000, 12) or 2000).}
  \item{freq}{The frequency of the time series (e.g., 4 for quarterly, 12 for monthly, 1 for annual).}
}
\value{
  A numeric vector representing the time points of the generated time series.
}
\examples{
# Monthly dates from Jan 2000 to Dec 2000
dates(c(2000, 1), c(2000, 12), 12)

# Annual dates from 1990 to 2000
dates(1990, 2000, 1)
}
\keyword{ts}
\keyword{utilities}

