\name{import.tsformat}
\alias{import.tsformat}
\title{Import time series data}
\description{Import time series data stored in a .csv file, with the metadata (frequency and start date) stored in the first two rows. Currently works with monthly, quarterly, and annual data. Optionally, you can include a line of documentation (see details). }
\usage{
import.tsformat(fn, skip=0, header=FALSE, ...)
}
\details{
The first two lines hold the frequency and start date. If the series is monthly starting in January 1979, the file will look like this:
    
\code{
# frequency: 12\cr
# start: 1979 1\cr
1.5\cr
2.6\cr
.\cr
.\cr
.\cr
}

If the series is annual starting in 1934, the file will look like this:

\code{
# frequency: 1\cr
# start: 1934\cr
1.5\cr
2.6\cr
.\cr
.\cr
.\cr
}

There can optionally be a line of documentation below the frequency and start date. It's one line that starts with \code{# doc:}. The documentation, if present, goes to the next line feed, and is added as an attribute to the returned time series. For the previous example, it would look like this:

\code{
# frequency: 1\cr
# start: 1934\cr
# doc: This is a line of documentation. I can write as much as I want, as long as there is no line feed. If you need a line feed, you're doing something wrong. This should include information like the source of the data. It's not a novel.
1.5\cr
2.6\cr
.\cr
.\cr
.\cr
}

WARNING: \code{header=FALSE} by default, which is the opposite of \code{read.csv}. That's because it's much harder to identify errors if \code{header=TRUE} is incorrect (you lose the first observation) than if \code{header=FALSE} is incorrect (you're trying to read the variable name as an observation). This is a source of bugs when reading in data, because the default value of the argument is used any time the user forgets to specify it, and even a careful data analyst might not do a thorough check of the data every time it's read.
}
\arguments{
	\item{fn}{The name of a .csv file.}
	\item{skip}{Number of lines to skip in the file after removing the metadata in the first two lines.}
	\item{...}{Additional arguments passed on to \code{read.csv} and applied to the data after removing the metadata in the first two lines.}
}
\value{Returns a \code{ts} object holding the output of a call to \code{read.csv}. Then frequency and start date specified in the csv file are added to the return value. If a documentation string is included in line 3, it's added as an attribute, and can be read using \code{attr(v, "doc")}, where \code{v} is the name of the variable holding the data.}
\author{Lance Bachmeier}

