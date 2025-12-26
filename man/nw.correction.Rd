\name{nw.correction}
\alias{nw.correction}
\title{Newey-West Corrected Standard Errors}
\description{
  Calculates Newey-West corrected standard errors, t-statistics, and coefficients for a linear model fit, accounting for heteroskedasticity and autocorrelation.
}
\usage{
nw.correction(fit)
}
\arguments{
  \item{fit}{An \code{lm} object (the result of a linear model fit).}
}
\value{
  A matrix with columns for coefficients, Newey-West corrected standard errors, and t-statistics.
}
\details{
  This function uses \code{sandwich::NeweyWest} to compute heteroskedasticity and autocorrelation consistent (Newey-West) standard errors.
}
\seealso{
  \code{\link[sandwich]{NeweyWest}}
}
\keyword{models}
\keyword{robust}

