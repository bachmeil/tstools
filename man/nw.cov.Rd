\name{nw.cov}
\alias{nw.cov}
\title{Newey-West Covariance Matrix}
\description{
  Returns the Newey-West heteroskedasticity and autocorrelation consistent covariance matrix of the coefficients for a linear model fit.
}
\usage{
nw.cov(fit)
}
\arguments{
  \item{fit}{An \code{lm} object (the result of a linear model fit).}
}
\value{
  A matrix representing the Newey-West covariance matrix.
}
\seealso{
  \code{\link[sandwich]{NeweyWest}}}
\keyword{models}
\keyword{robust}

