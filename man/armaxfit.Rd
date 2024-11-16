\name{armaxfit}
\alias{armaxfit}
\title{Estimate an ARMAX model}
\description{Estimates an ARMAX model}
\usage{armaxfit(y, xreg=NULL, ar=0, ma=0, trend=0, seasonal=FALSE,
auto=FALSE)}
\arguments{
	\item{y}{ts object}
	\item{xreg}{A vector or matrix of exogenous regressors. Note that
		\code{xreg} should not include polynomial time trend terms or
		seasonal dummies. Those are handled by options \code{trend} and
		\code{seasonal}, which are more convenient for prediction.}
	\item{ar}{Number of AR lags}
	\item{ma}{Number of MA lags}
	\item{trend}{Degree of polynomial trend to include. Defaults to 0 (no
		trend).}
	\item{seasonal}{Set to TRUE to add a seasonal dummy variable if you
		are using monthly or quarterly data. If you are using a different
		frequency of data such as weekly, you should use a procedure such as
		STL to seasonally adjust the data before estimating the model.}
	\item{auto}{Not yet implemented}
}
\value{Returns an S3 class object of type \code{armaxfit}. Is generally
	only of interest for prediction purposes using \code{predict} or
	\code{predictions}. Note that this function calls into base R's
	\code{arima}. It estimates a linear regression model with ARMA
	forecast errors. If you want to work with the coefficients of an ARX
	model, estimate the model manually using \code{tsreg}, as that will
	give you that coefficients you're after.}
\author{Lance Bachmeier}
