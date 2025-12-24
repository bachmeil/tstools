\name{vec.list}
\alias{vec.list}
\title{Coerce a List to a Numeric Vector}
\description{
  Coerces a list of numeric vectors into a single numeric vector.
}
\usage{
\method{vec}{list}(x)
}
\arguments{
  \item{x}{A list of numeric vectors.}
}
\value{
  A numeric vector formed by unlisting \code{x}.
}
\seealso{
\code{\link{vec}}
}
\examples{
lst <- list(1:3, 4:6)
vec(lst)
}
\keyword{methods}
\keyword{utilities}

