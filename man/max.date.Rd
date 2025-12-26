\name{max.date}
\alias{max.date}
\title{Determine the Maximum of Two Dates}
\description{
  Compares two dates (which can be numeric vectors representing year and period) and returns the later of the two.
}
\usage{
max.date(d1, d2)
}
\arguments{
  \item{d1}{The first date. Can be a numeric scalar (for annual data) or a numeric vector of length 2 (for year and period).}
  \item{d2}{The second date. Must be of the same format and length as \code{d1}.}
}
\value{
  The later of the two dates, in the same format as the input.
}
\examples{
  max.date(c(2000, 1), c(2000, 7)) # Returns c(2000, 7)
  max.date(2000, 2005)            # Returns 2005
}
\keyword{utilities}
\keyword{date}

