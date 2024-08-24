\name{armafit}
\alias{armafit}
\title{Estimate an ARMA model}
\description{Estimates an ARMA model. Optionally selects the lag length automatically using auto.arima in the forecast package.}
\usage{armafit(x, ar=0, ma=0, auto=FALSE)}
\arguments{
	\item{x}{ts object}
	\item{ar}{Number of AR lags if auto=FALSE, otherwise the maximum number of AR lags to try}
	\item{ma}{Number of MA lags if auto=FALSE, otherwise the maximum number of MA lags to try}
	\item{auto}{If TRUE, uses auto.arima to select the lag length}
}
\value{Returns the same output as a call to arima, but with an additional argument, par, that has an intercept with the conventional interpretation. The intercept in a call to arima is actually the mean, which is quite confusing for someone learning time series, and is honestly just a terrible label for that statistic.}
\author{Lance Bachmeier}
\examples{
set.seed(400)
x <- arima.sim(n=250, list(ar=c(0.5, 0.2, 0.1), ma=0.4))
fit <- armafit(x, ar=3, ma=3, auto=TRUE)
fit$par
} 
