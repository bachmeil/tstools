\name{month}
\alias{month}
\title{Extract Month from Time Series}
\description{
  Extracts the month number (1-12) from a time series object.
}
\usage{
month(x)
}
\arguments{
  \item{x}{A time series object.}
}
\value{
  An integer representing the month (1-12).
}
\examples{
  x <- ts(rnorm(24), start=c(2000, 1), frequency=12)
  month(x)
}
\keyword{utilities}
\keyword{time series}
