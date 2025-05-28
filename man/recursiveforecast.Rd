\name{recursive.forecast}
\alias{recursive.forecast}
\title{Make pseudo-OOS forecasts}
\description{
  Calculate pseudo-OOS forecasts to be used in an out-of-sample forecast evaluation or comparison. Forecasts are calculated recursively using the data that would have been available at the time the forecast would have been made. Supports increasing or fixed window estimation.
}
\usage{
recursive.forecast(data, f, pct, first.date, P, h, window)
}
\arguments{
  \item{data}{[ts] The variable being forecast}
  \item{f}{[function] The function that takes the data and creates a forecast}
  \item{pct}{[double] If provided, the percentage of the sample to use for out-of-sample evaluation}
  \item{first.date}{[date] If provided and pct is not, the first date to make a forecast}
  \item{P}{[int] If provided and pct and first.date are not, the number of observations to use for out-of-sample evaluation}
  \item{h}{[int=1] Forecast horizon. If set, it affects the data that's passed to f.}
  \item{window}{[int] If provided, use a rolling window with this many observations. Otherwise use an increasing window with all observations available at the time the forecast is made.}
}
\value{
  [ts] A time series that holds all the pseudo-OOS forecasts made
}
\examples{

}
\keyword{forecast, evaluation, recursive, out-of-sample}

