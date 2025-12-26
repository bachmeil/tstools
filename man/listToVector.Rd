\name{listToVector}
\alias{listToVector}
\title{Convert a List of Scalar Elements to a Vector}
\description{
  Converts a list where all elements are scalars of the same type (numeric, logical, or character) into a single vector. Provides options for handling empty lists, NA values, and NULL values, and for converting integers to numerics if mixed types are present.
}
\usage{
listToVector(xs, empty = "null", 
             na.action = c("error", "warn", "omit", "ignore"), 
             null.action = c("error", "warn", "omit"),
             convert.int = FALSE)
}
\arguments{
  \item{xs}{A list whose elements are intended to be scalars of the same type.}
  \item{empty}{What to return if the input list \code{xs} is empty. Can be \code{"error"} (to stop with an error), \code{"null"} (to return \code{NULL}), or any other value to be returned directly, such as \code{NA} or \code{numeric(0)}.}
  \item{na.action}{Action to take if \code{NA} values are present in \code{xs}. Options are \code{"error"} (stop), \code{"warn"} (warn and pass through), \code{"omit"} (remove \code{NA} values), or \code{"ignore"} (pass through silently).}
  \item{null.action}{Action to take if \code{NULL} values are present in \code{xs}. Options are \code{"error"} (stop), \code{"warn"} (warn and remove), or \code{"omit"} (remove silently).}
  \item{convert.int}{A logical value. If \code{TRUE}, and the list contains a mix of integer and double values, integers will be converted to numeric to form a single numeric vector. If \code{FALSE} (default), mixing integer and double types will result in an error.}
}
\value{
  A vector of the appropriate type (numeric, logical, or character) containing the elements of the input list. Returns \code{NULL} or the value of \code{empty} if the list is empty (depending on \code{empty} argument).
}
\details{
  This function is strict about input types and lengths:
  \itemize{
    \item All elements of the list must be scalars (length 1).
    \item \code{integer} and \code{double} elements cannot be mixed unless \code{convert.int = TRUE}.
    \item All elements must be the same type, unless \code{convert.int = TRUE} (in which case only \code{integer} and \code{double} elements can be mixed).
    \item Only four types are allowed: double, integer, logical, and character. Anything else results in an error.
    \item It handles \code{NA} and \code{NULL} values according to the specified \code{na.action} and \code{null.action}. The default is to die with an error if any elements of \code{xs} are \code{NA} or \code{NULL}.
  }
}
\examples{
  # Example with numeric data
  num_list <- list(1, 2, 3.5, 4L)
  listToVector(num_list, convert.int = TRUE) # Returns a numeric vector

  # Example with character data
  char_list <- list("a", "b", "c")
  listToVector(char_list) # Returns a character vector

  # Example with NA values (omit)
  na_list <- list(1, NA, 3)
  listToVector(na_list, na.action = "omit")

  # Example with NULL values (warn and omit)
  null_list <- list(1, NULL, 3)
  listToVector(null_list, null.action = "warn")

  # Example with an empty list
  empty_list <- list()
  listToVector(empty_list, empty = "null")
}
\keyword{utilities}
\keyword{list}
\keyword{vector}
