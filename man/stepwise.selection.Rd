\name{stepwise.selection}
\alias{stepwise.selection}
\title{Downward Stepwise Predictor Selection}
\description{
  Performs a downward stepwise (backward elimination) selection of predictors for a time series regression model.
}
\usage{
stepwise.selection(y, x, crit=BIC)
}
\arguments{
  \item{y}{The dependent variable (a time series).}
  \item{x}{An \code{mts} object of predictor variables.}
  \item{crit}{The criterion function to use for model selection. Defaults to \code{BIC}.}
}
\value{
  Prints the selection process and returns a message upon successful completion. The final selected variables are determined by the iterative process.
}
\details{
  This function starts with a full model and iteratively removes the least useful predictor according to the specified criterion, using the \code{drop.predictor} function.
}
\seealso{
  \code{\link{drop.predictor}}
}
\keyword{models}
\keyword{regression}

