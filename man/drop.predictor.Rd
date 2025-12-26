\name{drop.predictor}
\alias{drop.predictor}
\title{Drop Predictor from a Model Based on a Criterion}
\description{
  Selects the best model by dropping one predictor variable at a time and evaluating a given criterion (e.g., AIC, BIC).
}
\usage{
drop.predictor(y, x, crit)
}
\arguments{
  \item{y}{The dependent variable (a time series).}
  \item{x}{An \code{mts} object of predictor variables.}
  \item{crit}{A function that takes a model object and returns a numeric criterion value (lower is better).}
}
\value{
  An \code{mts} object containing the subset of predictors from the best model.
}
\details{
  This function iterates through the columns of \code{x}, dropping one at a time, fitting a model, and calculating the specified criterion. It is used internally by \code{stepwise.selection}.
}
\keyword{models}
\keyword{regression}

