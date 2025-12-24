\name{extract.quarter}
\alias{extract.quarter}
\title{Extract Data for a Specific Quarter}
\description{
  Extracts all observations corresponding to a specific quarter from a quarterly time series.
}
\usage{
extract.quarter(x, k)
}
\arguments{
  \item{x}{A quarterly time series object.}
  \item{k}{An integer (1-4) representing the desired quarter.}
}
\value{
  A time series object containing only the observations for the specified quarter, with an annual frequency.
}
\examples{
  x <- ts(rnorm(12), start=c(2000, 1), frequency=4)
  extract.quarter(x, 1) # Q1 data
}
\keyword{utilities}
\keyword{time series}

