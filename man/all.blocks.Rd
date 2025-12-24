\name{all.blocks}
\alias{all.blocks}
\title{Get All Blocks of a Time Series}
\description{
  Returns a list of all contiguous blocks of a specified length from a time series.
}
\usage{
all.blocks(x, k)
}
\arguments{
  \item{x}{A \code{ts} or \code{mts} object.}
  \item{k}{The length of the blocks to extract.}
}
\value{
  A list of \code{ts} or \code{mts} objects, where each element is a block of length \code{k} from the original series \code{x}.
}
\examples{
y <- ts(1:10, start=2001)
all.blocks(y, 3)
}
\keyword{ts}
\keyword{utilities}

