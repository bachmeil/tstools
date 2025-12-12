\name{tail.mts}
\alias{tail.mts}
\title{Extract the Last Part of a Multiple Time Series Object}
\description{
  An S3 method for the generic \code{\link{tail}} function, specifically for objects of class \code{"mts"}
  (multiple time series). It returns the last \code{n} observations of a multiple time series,
  preserving its time series attributes. This function is an alias for \code{\link{tail.ts}}.
}
\usage{
\method{tail}{mts}(x, n = 6L)
}
\arguments{
  \item{x}{A multiple time series object (i.e., an object of class \code{"mts"}).}
  \item{n}{A single integer. The number of observations to return from the end of the time series. Defaults to 6L.}
}
\value{
  A multiple time series object (\code{"mts"}) containing the last \code{n} observations of \code{x}.
}
\seealso{
\code{\link{tail}}, \code{\link{tail.ts}}, \code{\link{head.mts}}}
\examples{
# Create a sample mts object
data(presidents)
pres_mts <- ts(cbind(presidents, diff(presidents)), start=start(presidents), frequency=frequency(presidents))
colnames(pres_mts) <- c("Approval", "Change")

tail(pres_mts)
tail(pres_mts, n = 10)
}
\keyword{ts}
\keyword{utilities}

