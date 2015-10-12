\name{itemFrequency}
\alias{itemFrequency}
\alias{itemFrequency,itemMatrix-method}
\alias{itemFrequency,tidLists-method}
\title{Getting Frequency/Support for Single Items}
\description{
  Provides the generic function \code{itemFrequency} and S4 methods to get the
  frequency/support for all single items in an objects based on
  \code{\linkS4class{itemMatrix}}.  For example, it is used to get the single
  item support from an object of class \code{\linkS4class{transactions}}
  without mining.
}
\usage{
itemFrequency(x, ...)

\S4method{itemFrequency}{itemMatrix}(x, type, weighted = FALSE)
}
\arguments{
  \item{x}{an object.}
  \item{\dots}{ further arguments are passed on.}
  \item{type}{ a character string specifying 
    if \code{"relative"}  frequency/support or  \code{"absolute"} 
    frequency/support (item counts) is returned. (default:  \code{"relative"}).
  }
  \item{weighted}{ should support be weighted by transactions weights stored
   as column \code{"weight"} in transactionInfo?} 
}
\value{
  \code{itemFrequency} returns a named numeric vector.  Each element is the
  frequency/support of the corresponding item in object \code{x}.  The items
  appear in the vector in the same order as in the binary matrix in \code{x}.
}
\seealso{ 
  \code{\link{itemFrequencyPlot}},
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")
itemFrequency(Adult, type = "relative")
}
\keyword{models}
