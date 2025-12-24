\name{tsToList}
\alias{tsToList}
\title{Convert a Time Series Object to a List}
\description{
  Converts a univariate or multivariate time series object (\code{ts} or \code{mts}) into a list.
}
\usage{
tsToList(x)
}
\arguments{
  \item{x}{A \code{ts} or \code{mts} object.}
}
\value{
  A list. If \code{x} is a univariate \code{ts} object, the list will have one element containing the series, and the list element will be named after the variable \code{x}. If \code{x} is an \code{mts} object, the list will have as many elements as \code{x} has columns, with each element being one of the columns and named accordingly.
}
\seealso{
\code{\link{with.ts}}
}
\examples{
y <- ts(rnorm(10), start=2000)
tsToList(y)

data <- ts.intersect(gdp=ts(rnorm(10), start=2000),
                     inf=ts(rnorm(10), start=2000))
tsToList(data)
}
\keyword{ts}
\keyword{utilities}

