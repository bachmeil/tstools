\name{quarter.dummy}
\alias{quarter.dummy}
\title{Generate Quarterly Dummy Variables}
\description{
  Generates a time series of quarterly dummy variables (0 or 1) for a given quarterly time series.
  Allows one quarter to be omitted (e.g., for use as a reference category in regression).
}
\usage{
quarter.dummy(x, omit=1)
}
\arguments{
  \item{x}{A quarterly time series object.}
  \item{omit}{Optional: An integer (1-4) representing the quarter to omit. Defaults to 1.}
}
\value{
  An \code{mts} object containing the quarterly dummy variables.
}
\examples{
  x <- ts(rnorm(12), start=c(2000, 1), frequency=4)
  quarter.dummy(x, omit=4)
}
\keyword{utilities}
\keyword{time series}

