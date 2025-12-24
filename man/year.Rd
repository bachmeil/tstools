\name{year}
\alias{year}
\title{Extract Year from Time Series}
\description{
  Extracts the year from a time series object.
}
\usage{
year(x)
}
\arguments{
  \item{x}{A time series object.}
}
\value{
  An integer representing the year.
}
\examples{
  x <- ts(rnorm(12), start=c(2000, 1), frequency=4)
  year(x)
}
\keyword{utilities}
\keyword{time series}

