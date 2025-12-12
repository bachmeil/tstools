\name{rest}
\alias{rest}
\title{Get All Elements Except the First}
\description{
  Returns all elements of a vector or time series object except for the first one.
  This function is a convenience wrapper around \code{tail(x, -1)}.
}
\usage{
rest(x)
}
\arguments{
  \item{x}{A vector or a time series object.}
}
\value{
  An object of the same class as \code{x} with the first element removed.
}
\seealso{
\code{\link{first}}, \code{\link{drop_first}}
}
\examples{
vec <- 1:5
rest(vec)

ts_object <- ts(1:10, start = c(2000, 1), frequency = 4)
rest(ts_object)
}
\keyword{utils}

