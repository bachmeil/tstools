\name{tsobs}
\alias{tsobs}
\title{Extract a Single Observation from a Time Series}
\description{
  Extracts a single observation from a time series at a specified date.
}
\usage{
tsobs(x, d, type = "ts")
}
\arguments{
  \item{x}{A \code{ts} or \code{mts} object.}
  \item{d}{The date at which to extract the observation (e.g., c(1990, 1) or 1990.0).}
  \item{type}{The desired return type: "ts" for a time series object (default) or "numeric" for a numeric vector.}
}
\value{
  Either a \code{ts} object (univariate or multivariate) or a numeric vector, containing the observation(s) at date \code{d}.
}
\examples{
y <- ts(rnorm(100), start=c(1990,1), frequency=12)

# Extract as a time series object
tsobs(y, c(1995, 6), type="ts")

# Extract as a numeric value
tsobs(y, c(1995, 6), type="numeric")

mts_data <- ts.intersect(y, x=lag(y, -1))
# Extract a row from an mts object as a ts object
tsobs(mts_data, c(1995, 6), type="ts")

# Extract a row from an mts object as numeric values
tsobs(mts_data, c(1995, 6), type="numeric")
}
\keyword{ts}
\keyword{utilities}

