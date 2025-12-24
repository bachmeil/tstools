\name{quarter}
\alias{quarter}
\title{Extract Quarter from Time Series}
\description{
  Extracts the quarter number (1-4) from a time series object.
}
\usage{
quarter(x)
}
\arguments{
  \item{x}{A time series object.}
}
\value{
  An integer representing the quarter (1-4).
}
\examples{
  x <- ts(rnorm(12), start=c(2000, 1), frequency=4)
  quarter(x)
}
\keyword{utilities}
\keyword{time series}

