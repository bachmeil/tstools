\name{min.date}
\alias{min.date}
\title{Determine the Minimum of Two Dates}
\description{
  Compares two dates (which can be numeric vectors representing year and period) and returns the earlier of the two.
}
\usage{
min.date(d1, d2)
}
\arguments{
  \item{d1}{The first date. Can be a numeric scalar (for annual data) or a numeric vector of length 2 (for year and period).}
  \item{d2}{The second date. Must be of the same format and length as \code{d1}.}
}
\value{
  The earlier of the two dates, in the same format as the input.
}
\examples{
  min.date(c(2000, 1), c(2000, 7)) # Returns c(2000, 1)
  min.date(2000, 2005)            # Returns 2000
}
\keyword{utilities}
\keyword{date}

