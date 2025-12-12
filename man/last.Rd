\name{last}
\alias{last}
\title{Get the Last N Elements}
\description{
  Returns the last \code{n} non-missing elements of a vector or time series object.
  For time series objects, it acts as a wrapper for \code{\link{tail}}. For vectors,
  it first removes \code{NA} values before extracting the last \code{n} elements.
}
\usage{
last(x, n = 1L)
}
\arguments{
  \item{x}{A vector or a time series object.}
  \item{n}{A single integer. The number of elements to return. Defaults to 1L.}
}
\value{
  An object of the same class as \code{x} containing the last \code{n} non-missing elements.
}
\seealso{
\code{\link{first}}, \code{\link{head}}, \code{\link{tail}}
}
\examples{
vec <- c(1, 2, NA, 3, 4, 5)
last(vec)
last(vec, 3)

ts_object <- ts(c(1:9, NA), start = c(2000, 1), frequency = 4)
last(ts_object)
last(ts_object, 4)
}
\keyword{utils}

