\name{drop_first}
\alias{drop_first}
\title{Drop the First N Elements}
\description{
  Removes the first \code{n} elements from a vector or time series object.
}
\usage{
drop_first(x, n = 1L)
}
\arguments{
  \item{x}{A vector or a time series object.}
  \item{n}{A single integer greater than zero. The number of initial elements to drop. Defaults to 1L.}
}
\value{
  An object of the same class as \code{x} with the first \code{n} elements removed.
}
\seealso{
\code{\link{drop_last}}, \code{\link{first}}, \code{\link{tail}}}
\examples{
vec <- 1:5
drop_first(vec)
drop_first(vec, 3)

ts_object <- ts(1:10, start = c(2000, 1), frequency = 4)
drop_first(ts_object, 2)
}
\keyword{utils}

