\name{\%++\%}
\alias{\%++\%}
\alias{\%++\%.ts}
\alias{\%++\%.default}
\title{Append to Time Series Object (Shorthand)}
\description{
  A shorthand operator for appending data to a time series (ts) object or a default vector.
}
\usage{
x \%++\% y
\method{\%++\%}{ts}(x, y)
\method{\%++\%}{default}(x, y)
}
\arguments{
  \item{x}{The time series object or vector to which \code{y} will be appended.}
  \item{y}{The data to append. Can be a numeric value, vector, or time series.}
}
\value{
  If \code{x} is a \code{ts} object, a new \code{ts} object with \code{y} appended.
  If \code{x} is a default vector, a combined vector \code{c(x, y)}.
}
\details{
  This operator provides a convenient way to extend a time series or vector.
  \itemize{ 
    \item If \code{x} is a \code{ts} object, \code{y} is appended as numeric values, and the result is a new \code{ts} object with the same start date and frequency as \code{x}.
    \item If \code{x} is not a \code{ts} object, it defaults to standard vector concatenation using \code{c(x, y)}.
  }
}
\examples{
  z1 <- ts(1:5, start=c(2000, 1), frequency=12)
  z_extended <- z1 \%++\% 6
  print(z_extended)

  vec <- c(1, 2, 3)
  vec_extended <- vec \%++\% 4
  print(vec_extended)
}
\keyword{utilities}
\keyword{time series}
\keyword{operators}
