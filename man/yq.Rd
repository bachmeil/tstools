\name{yq}
\alias{yq}
\title{Combine Year and Quarter}
\description{
  Combines the year and quarter of a time series object into a single integer (e.g., 2000Q1 becomes 200001).
}
\usage{
 yq(x)
}
\arguments{
  \item{x}{A time series object (typically quarterly).}
}
\value{
  An integer representing the combined year and quarter.
}
\examples{
  x <- ts(rnorm(12), start=c(2000, 1), frequency=4)
  yq(x)
}
\keyword{utilities}
\keyword{time series}

