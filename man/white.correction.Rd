\name{white.correction}
\alias{white.correction}
\title{White-Corrected Standard Errors}
\description{
  Calculates White-corrected standard errors, t-statistics, and coefficients for a linear model fit.
}
\usage{
white.correction(fit)
}
\arguments{
  \item{fit}{An \code{lm} object (the result of a linear model fit).}
}
\value{
  A matrix with columns for coefficients, White-corrected standard errors, and t-statistics.
}
\details{
  This function uses \code{sandwich::vcovHC} to compute heteroskedasticity-consistent (White) standard errors.
}
\seealso{
  \code{\link[sandwich]{vcovHC}}
}
\keyword{models}
\keyword{robust}

