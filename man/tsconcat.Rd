\name{\%~\%}
\alias{\%~\%}
\alias{\%~\%.ts}
\alias{\%~\%.NULL}
\title{Combine Time Series Objects (Shorthand for ts.combine)}
\description{
  A shorthand operator for combining time series (ts) or multiple time series (mts) objects, similar to \code{\link{ts.combine}}.
}
\usage{
x \%~\% y
\method{\%~\%}{ts}(x, y)
\method{\%~\%}{NULL}(x, y)
}
\arguments{
  \item{x}{The first time series object (can be \code{NULL}, \code{ts}, or \code{mts}).}
  \item{y}{The second time series object (can be \code{ts} or \code{mts}).}
}
\value{
  A combined time series object (\code{ts} or \code{mts}).
}
\details{
  This operator provides a convenient way to horizontally combine time series objects.
  \itemize{
    \item If \code{x} is \code{NULL}, it returns \code{y} (if \code{y} is a time series) or \code{NULL}.
    \item If both \code{x} and \code{y} are time series, they are combined using \code{\link{ts.combine}}.
    \item Column names are derived from the original objects or the deparsed names of the arguments.
  }
}
\seealso{
  \code{\link{ts.combine}}
}
\examples{
  z1 <- ts(rnorm(10), start=c(2000, 1), frequency=12)
  z2 <- ts(rnorm(10), start=c(2000, 1), frequency=12)
  combined_ts <- z1 \%~\% z2
  print(combined_ts)

  # Example with NULL
  null_ts <- NULL \%~\% z1
  print(null_ts)
}
\keyword{utilities}
\keyword{time series}
\keyword{operators}
