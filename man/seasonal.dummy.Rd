\name{seasonal.dummy}
\alias{seasonal.dummy}
\title{Generate Seasonal Dummy Variables}
\description{
  Generates seasonal dummy variables (monthly or quarterly) for a given time series.
}
\usage{
seasonal.dummy(x, omit=1)
}
\arguments{
  \item{x}{A time series object (monthly or quarterly).}
  \item{omit}{Optional: For monthly, a month name or number to omit. For quarterly, an integer (1-4) to omit. Defaults to 1.}
}
\value{
  An \code{mts} object of seasonal dummy variables.
}
\examples{
  x <- ts(rnorm(24), start=c(2000, 1), frequency=12)
  seasonal.dummy(x, omit="Dec")
}
\keyword{utilities}
\keyword{time series}

