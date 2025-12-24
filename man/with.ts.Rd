\name{with.ts}
\alias{with.ts}
\title{Evaluate an Expression in a Time Series Environment}
\description{
  This is a method for the generic function \code{with} for time series objects. It evaluates an R expression in an environment constructed from a time series, potentially using column names as variables.
}
\usage{
\method{with}{ts}(x, expr)
}
\arguments{
  \item{x}{A \code{ts} or \code{mts} object.}
  \item{expr}{The expression to be evaluated.}
}
\details{
  \code{with.ts} temporarily converts the time series object into a list using \code{\link{tsToList}} and then calls the standard \code{with} function. This allows you to refer to the columns of a multivariate time series (\code{mts}) by name within the expression. For a univariate time series, you can refer to it by the name of the object itself.
}
\value{
  The result of evaluating \code{expr}.
}
\seealso{
\code{\link{with}}, \code{\link{tsToList}}
}
\examples{
data <- ts.intersect(gdp=ts(1:10, start=2000),
                    consumption=ts(log(1:10), start=2000))

with.ts(data, gdp / consumption)

y <- ts(rnorm(10, 5), start=c(1990,1))
with.ts(y, mean(y))
}
\keyword{ts}
\keyword{utilities}

