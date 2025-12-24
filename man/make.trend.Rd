\name{make.trend}
\alias{make.trend}
\title{Create a Time Trend Variable}
\description{
  Creates a time trend variable or a matrix of polynomial time trends.
}
\usage{
make.trend(x, k = 1)
}
\arguments{
  \item{x}{A time series object. The data in the object is not used, but its time series properties (start, end, frequency) are.}
  \item{k}{An integer specifying the highest power of the trend to create. If k=1 (the default), a simple linear trend is created. If k > 1, a matrix with columns for trend, trend^2, ..., trend^k is created.}
}
\value{
  A time series object containing the trend(s). If k=1, it's a univariate time series. If k>1, it's a multivariate time series (\code{mts}) object.
}
\examples{
y <- ts(rnorm(100), start=c(1990,1), frequency=12)
trend <- make.trend(y)
plot(trend)

trends <- make.trend(y, k=3)
head(trends)
}
\keyword{ts}

