\name{drop_last}
\alias{drop_last}
\title{Drop the Last N Elements}
\description{
  Removes the last \code{n} elements from a vector or time series object.
}
\usage{
drop_last(x, n = 1L)
}
\arguments{
  \item{x}{A vector or a time series object.}
  \item{n}{A single integer greater than zero. The number of trailing elements to drop. Defaults to 1L.}
}
\value{
  An object of the same class as \code{x} with the last \code{n} elements removed.
}
\seealso{
\code{\link{drop_first}}, \code{\link{last}}, \code{\link{head}}
}
\examples{
vec <- 1:5
drop_last(vec)
drop_last(vec, 2)

ts_object <- ts(1:10, start = c(2000, 1), frequency = 4)
drop_last(ts_object, 3)
}
\keyword{utils}

