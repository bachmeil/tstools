\name{pred_month}
\alias{pred_month}
\title{Generate Prediction Vector for a Specific Month Dummy}
\description{
  A helper function for forecasting, generating a vector of zeros with a one at the position corresponding to the specified month dummy.
}
\usage{
pred_month(dum, month)
}
\arguments{
  \item{dum}{A matrix of dummy variables (e.g., from \code{month.dummy}).}
  \item{month}{A character string (e.g., "Jan") representing the target month.}
}
\value{
  A numeric vector of length 11 (assuming one month is omitted).
}
\keyword{internal}
\keyword{utilities}

