\name{extract.month}
\alias{extract.month}
\title{Extract Data for a Specific Month}
\description{
  Extracts all observations corresponding to a specific month from a monthly time series.
}
\usage{
extract.month(x, k)
}
\arguments{
  \item{x}{A monthly time series object.}
  \item{k}{An integer (1-12) representing the desired month.}
}
\value{
  A time series object containing only the observations for the specified month, with an annual frequency.
}
\examples{
  x <- ts(rnorm(24), start=c(2000, 1), frequency=12)
  extract.month(x, 1) # January data
}
\keyword{utilities}
\keyword{time series}

