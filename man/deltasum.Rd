\name{delta.sum}
\alias{delta.sum}
\alias{delta.cumsum}
\alias{se.sum}
\alias{se.cumsum}
\title{Standard error of a sum of coefficients}
\description{Calculates the standard error of a sum of coefficients using the delta method.}
\usage{
delta.sum(theta, v)
delta.cumsum(theta, v)
se.sum(fit, m=NULL)
se.cumsum(fit, m=NULL)
}
\details{
    \code{delta.sum} and \code{delta.cumsum} take a vector of coefficients and a matrix of covariance terms as arguments and return the standard error of the sum and cumulative sum, respectively, of those coefficients.
    
    \code{se.sum} and \code{se.cumsum} do the same thing, but the arguments are a fitted \code{lm} object and an index indicating the location of the coefficients used to calculate the sum.
    
    \code{se.sum} and \code{se.cumsum} are more convenient when they work, but \code{delta.sum} and \code{delta.cumsum} will work in any case that you have a coefficient vector and a coefficient covariance matrix.
}
\arguments{
	\item{theta}{Vector of coefficients}
	\item{v}{Matrix holding covariance terms corresponding to theta}
	\item{fit}{An estimated lm object}
	\item{m}{A vector holding the index values of the coefficients of fit that are of interest. If not specified, it will select all coefficients.}
}
\value{Returns a standard error for the sum of coefficients when taking the sum, and a vector of standard errors when taking the cumulative sum. The standard error is calculated using the \code{deltamethod} function from the msm package.}
\author{Lance Bachmeier}
\examples{
    set.seed(200)
    y <- rnorm(400)
    x <- matrix(rnorm(1200), ncol=3)
    fit <- lm(y ~ x)
    # Calculate the standard error of the sum of all coefficients
    # including the intercept
    delta.sum(coef(fit), vcov(fit))
    # Same thing, but se.sum will extract the coefficients and by default
    # takes the sum of all coefficients
    se.sum(fit)
    # Standard error of the sum of only the slope coefficients
    delta.sum(coef(fit)[2:4], vcov(fit)[2:4, 2:4])
    se.sum(fit, 2:4)
    # Vector of standard errors of the cumulative sums
    delta.cumsum(coef(fit)[2:4], vcov(fit)[2:4, 2:4])
    se.cumsum(fit, 2:4)
}
