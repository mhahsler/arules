\name{size}
\alias{size}
\alias{size,itemMatrix-method}
\alias{size,itemsets-method}
\alias{size,rules-method}
\alias{size,tidLists-method}
\title{Number of Items}
\description{
  Provides the generic function \code{size} and S4 methods to get the size of
  each element from objects based on \code{\linkS4class{itemMatrix}}.  For
  example, it is used to get a vector of transaction sizes (i.e., the number of
  present items (ones) per element (row) of the binary incidence matrix) from
  an object of class \code{\linkS4class{transactions}}).
}
\usage{
size(x, ...)
}
\arguments{
  \item{x}{an object.}
  \item{\ldots}{further (unused) arguments.}
}
\value{
  \code{size} returns a numeric vector of length \code{length(x)}.  Each
  element is the size of the corresponding element (row in the matrix) in
  object \code{x}. For rules, \code{size} returns the sum of the number of
  elements in the LHS and the RHS.
}
\seealso{ 
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")
summary(size(Adult))
}
\keyword{attribute}
