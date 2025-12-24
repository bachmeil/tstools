\name{residuals.tsreg}
\alias{residuals.tsreg}
\title{Extract Residuals from a tsreg Object}
\description{
  Extracts the residuals from a time series regression model fitted by \code{tsreg}.
}
\usage{
\method{residuals}{tsreg}(x)
}
\arguments{
  \item{x}{An object of class \code{tsreg}, the result of a call to \code{\link{tsreg}}.}
}
\details{
  This is the residuals method for \code{tsreg} objects. It returns the residuals as a time series (\code{ts}) object, preserving the time series properties of the original data.
}
\value{
  A time series (\code{ts}) object containing the model residuals.
}
\seealso{
\code{\link{tsreg}}
}
\examples{
y <- ts(rnorm(100), start=c(1990,1), frequency=12)
x <- lag(y, -1)
fit <- tsreg(y, x)
res <- residuals(fit)
class(res)
tsinfo(res)
}
\keyword{ts}
\keyword{residuals}

