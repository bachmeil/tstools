\name{hstep}
\alias{hstep}
\title{Estimate a forecasting model using an h-step ahead projection}
\description{Estimate an h-step ahead forecasting model using direct estimation.}
\usage{hstep(lhs, rhs, k=1, h=1)}
\arguments{
	\item{lhs}{A ts object that is the dependent variable in the regression}
	\item{rhs}{A ts object or mts object holding the regressors.}
	\item{k}{The number of lags of each element of rhs that will be used in the regression.}
	\item{h}{The horizon of the regression. If h=3, then the time t values of lhs will be regressed on the time t-3 and later values of rhs.}
  \item{ect}{If there is an error correction term in the model, or multiple error correction terms, they should be added here. Note that this term should not be lagged - the function handles all lagging.}
}
\value{Returns a list of class hstep holding:
\item{fit}{The output returned by tsreg after estimating the model.}
\item{rhs}{}
\item{k}{}
\item{h}{}
}
\author{Lance Bachmeier}
\details{There is a `predict` method for the hstep object: `predict(fit)`.}
\examples{
}
