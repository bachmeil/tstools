\name{month.dummy}
\alias{month.dummy}
\title{Generate Monthly Dummy Variables}
\description{
  Generates a time series of monthly dummy variables (0 or 1) for a given monthly time series.
  Allows one month to be omitted (e.g., for use as a reference category in regression).
}
\usage{
month.dummy(x, omit=NULL)
}
\arguments{
  \item{x}{A monthly time series object.}
  \item{omit}{Optional: A character string (e.g., "Jan") or an integer (1-12) representing the month to omit. If \code{NULL}, all 12 dummies are returned.}
}
\value{
  An \code{mts} object containing the monthly dummy variables.
}
\examples{
  x <- ts(rnorm(24), start=c(2000, 1), frequency=12)
  month.dummy(x, omit="Jan")
}
\keyword{utilities}
\keyword{time series}

