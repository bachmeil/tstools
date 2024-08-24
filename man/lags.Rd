\name{lags}
\alias{lags}
\title{Create multiple lags of a ts object}
\description{Creates an mts object from a ts object and a vector of lags}
\usage{lags(x, k)}
\arguments{
	\item{x}{ts object}
	\item{k}{Vector of lags}
}
\value{Returns an mts object with lags of \code{x} as given by \code{k}. Follows the usual convention that a positive value in \code{k} means to take the lag rather than the lead, unlike R's builtin \code{lag} function. Observations with missing values at the beginning or end are dropped.}
\author{Lance Bachmeier}
\seealso{ \code{\link{lag}}, \code{\link{ts.intersect}} }
\examples{
y <- ts(rnorm(100))
lags(y, 1:12)
}