\name{predict.tsreg}
\alias{predict.tsreg}
\title{Predict Method for tsreg Objects}
\description{
  Obtains predictions from a fitted \code{tsreg} object.
}
\usage{
\method{predict}{tsreg}(obj, newdata = NULL)
}
\arguments{
  \item{obj}{An object of class \code{tsreg}, typically the result of a call to \code{\link{tsreg}}.}
  \item{newdata}{An optional data structure containing the values of the predictor variables for which to predict. This can be a vector, matrix, \code{mts}, data frame, or list. If omitted, the prediction is for the period immediately following the estimation sample, using the last available observation of the predictors.}
}
\details{
  This function is the predict method for objects returned by \code{tsreg}. If \code{newdata} is not provided, it produces a one-step-ahead forecast using the last observation in the model matrix of the fitted object. This is useful for h-step ahead forecasting schemes.
  
  If \code{newdata} is provided, the function computes the predictions based on the new data. The function handles the inclusion of an intercept automatically, based on the original model fit.
}
\value{
  A numeric value or vector of predicted values.
}
\seealso{
\code{\link{tsreg}}
}
\examples{
# Example usage:
y <- ts(rnorm(100), start=c(1990,1), frequency=4)
x <- lag(y, -1) # x is a predictor for y
fit <- tsreg(y, x, start=c(1991,1), end=c(2000,1))

# One-step-ahead forecast
predict(fit)

# Predict with new data
newdata <- ts(c(0.5, 0.8), start=c(2000,2), frequency=4)
predict(fit, newdata=newdata)
}
\keyword{ts}
\keyword{predict}
