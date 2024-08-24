\name{tsreg}
\alias{tsreg}
\title{Regression using time series data}
\description{Estimate a linear regression using time series data. Properly handles time series regressors and preserves the time series properties of the output.}
\usage{tsreg(y, x, start=NULL, end=NULL, intercept=TRUE)}
\arguments{
	\item{y}{A ts object that is the dependent variable in the regression}
	\item{x}{A ts object that holds the regressors. It can be either a single ts vector or a matrix of ts objects.}
	\item{start}{The start date for the regression sample. Matches the time index for y. If the first observation of the regression is later than start, this argument has no effect.}
	\item{end}{The end date for the regression sample. Matches the time index for y. If the last observation of the regression is earlier than start, this argument has no effect.}
	\item{intercept}{If FALSE, the intercept is equal to zero.}
}
\value{Returns a list holding everything returned by a call to lm, plus the following additional elements:
\item{fit}{lm output}
\item{resids}{Vector of residuals, with time series properties}
\item{fitted}{Vector of fitted values, with time series properties}
\item{start}{Start date for the estimation sample used in the regression, after eliminating rows with missing observations}
\item{end}{End date for the estimation sample used in the regression, after eliminating rows with missing observations}
\item{int}{TRUE if there is an intercept in the estimated model}
\item{nw}{The coefficients with Newey-West corrected standard errors and t-statistics}
}
\author{Lance Bachmeier}
\examples{
}
