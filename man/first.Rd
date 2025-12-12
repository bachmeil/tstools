\name{first}
\alias{first}
\title{Get the First N Elements}
\description{
  Returns the first n elements of a vector or time series. It is a wrapper for \code{head} for time series and a convenience function for vectors.
}
\usage{
first(x, n = 1)
}
\arguments{
  \item{x}{A vector or a time series object.}
  \item{n}{A single integer. The number of elements to return. Defaults to 1.}
}
\value{
  An object of the same class as \code{x} containing the first \code{n} elements.
}
\seealso{
\code{\link{last}}, \code{\link{head}}, \code{\link{tail}}}
\examples{
vec <- c(1, 2, 3, 4, 5)
first(vec)
first(vec, 3)

ts_object <- ts(1:10, start = c(2000, 1), frequency = 4)
first(ts_object)
first(ts_object, 4)
}
\keyword{utils}

