\name{tail.ts}
\alias{tail.ts}
\title{Extract the Last Part of a Time Series Object}
\description{
  An S3 method for the generic \code{\link{tail}} function, tailored for objects of class \code{"ts"}.
  It returns the last \code{n} observations of a time series, preserving its time series attributes.
}
\usage{
\method{tail}{ts}(x, n = 6L)
}
\arguments{
  \item{x}{A time series object (i.e., an object of class \code{"ts"}).}
  \item{n}{A single integer. The number of observations to return from the end of the time series. Defaults to 6L.}
}
\value{
  A time series object (\code{"ts"}) containing the last \code{n} observations of \code{x}.
}
\seealso{
\code{\link{tail}}, \code{\link{head.ts}}, \code{\link{last}}
}
\examples{
data(AirPassengers)
tail(AirPassengers)
tail(AirPassengers, n = 12)
}
\keyword{ts}
\keyword{utilities}
