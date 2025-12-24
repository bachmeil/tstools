\name{meanreg}
\alias{meanreg}
\title{Mean Regression}
\description{
  Computes the mean of a time series over a specified sample period by running a regression of the series on an intercept.
}
\usage{
meanreg(y, start = NULL, end = NULL)
}
\arguments{
  \item{y}{A univariate time series object.}
  \item{start}{The start date for the computation. If NULL, the start date of \code{y} is used.}
  \item{end}{The end date for the computation. If NULL, the end date of \code{y} is used.}
}
\details{
  This function is a simple wrapper around \code{\link{tsreg}}. It regresses the time series \code{y} on a constant. The result is an object of class \code{tsreg}, which inherits from \code{lm}. The main purpose is to provide a consistent interface for mean estimation that aligns with other regression functions in the \code{tstools} package.
}
\value{
  An object of class \code{"tsreg"}, which inherits from \code{"lm"}. The object contains the results of regressing \code{y} on a constant. The estimated coefficient for the intercept is the sample mean of \code{y} over the specified period.
}
\seealso{
\code{\link{tsreg}}
}
\examples{
y <- ts(rnorm(50, mean=5), start=c(2000, 1), frequency=4)
# Calculate the mean of the entire series
fit <- meanreg(y)
print(fit)
# The coefficient is the mean
coef(fit)

# Calculate the mean for a subset of the data
fit_sub <- meanreg(y, start=c(2005,1), end=c(2010,4))
cof(fit_sub)
mean(window(y, start=c(2005,1), end=c(2010,4)))
}
\keyword{ts}
