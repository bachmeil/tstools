\name{dummy.after}
\alias{dummy.after}
\title{Generate Seasonal Dummies for Forecast Horizon}
\description{
  Generates seasonal dummy variables (monthly or quarterly) for a forecast horizon starting after a given time series.
}
\usage{
dummy.after(x, n, omit=1)
}
\arguments{
  \item{x}{A time series object (monthly or quarterly).}
  \item{n}{The number of periods for the forecast horizon.}
  \item{omit}{Optional: For monthly, a month name or number to omit. For quarterly, an integer (1-4) to omit. Defaults to 1.}
}
\value{
  An \code{mts} object of seasonal dummy variables for the forecast horizon.
}
\examples{
  x <- ts(rnorm(24), start=c(2000, 1), frequency=12)
  dummy.after(x, 3)
}
\keyword{utilities}
\keyword{time series}
