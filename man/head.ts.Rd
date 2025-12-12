\name{head.ts}
\alias{head.ts}
\title{Extract the First Part of a Time Series Object}
\description{
  An S3 method for the generic \code{\link{head}} function, tailored for objects of class \code{"ts"}.
  It returns the first \code{n} observations of a time series, preserving its time series attributes.
}
\usage{
\method{head}{ts}(x, n = 6L)
}
\arguments{
  \item{x}{A time series object (i.e., an object of class \code{"ts"}).}
  \item{n}{A single integer. The number of observations to return from the beginning of the time series. Defaults to 6L.}
}
\value{
  A time series object (\code{"ts"}) containing the first \code{n} observations of \code{x}.
}
\seealso{
\code{\link{head}}, \code{\link{tail.ts}}, \code{\link{first}}}
\examples{
data(AirPassengers)
head(AirPassengers)
head(AirPassengers, n = 12)
}
\keyword{ts}
\keyword{utilities}

