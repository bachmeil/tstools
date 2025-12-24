\name{trend.after}
\alias{trend.after}
\title{Generate Trend Variables for Forecast Horizon}
\description{
  Generates trend variables for a specified forecast horizon, extending from an existing time series.
}
\usage{
trend.after(x, h)
}
\arguments{
  \item{x}{A time series object.}
  \item{h}{The number of periods for the forecast horizon.}
}
\value{
  A time series object containing the trend variables for the forecast horizon.
}
\examples{
  x <- ts(rnorm(24), start=c(2000, 1), frequency=12)
  trend.after(x, 3)
}
\keyword{utilities}
\keyword{time series}

