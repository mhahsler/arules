\name{is.superset}
\alias{is.subset}
\alias{is.subset,itemMatrix-method}
\alias{is.subset,associations-method}
\alias{is.superset}
\alias{is.superset,itemMatrix-method}
\alias{is.superset,associations-method}
%
%
\title{Find Super and Subsets}
\description{
  Provides the generic functions and the S4 methods \code{is.subset} and
  \code{is.superset} for finding super or subsets in associations and
  itemMatrix objects.
}
\usage{
is.subset(x, y = NULL, proper = FALSE, sparse = TRUE, ...)
is.superset(x, y = NULL, proper = FALSE, sparse = TRUE, ...)
}
\arguments{
  \item{x, y}{ associations or itemMatrix objects. If \code{y = NULL}, 
    the super or subset structure within set \code{x} is calculated.}
  \item{proper}{a logical indicating if all or just proper super or subsets.}
  \item{sparse}{a logical indicating if a sparse (ngCMatrix) rather than a 
  dense logical matrix sgould be returned. Sparse computation 
  preserves a significant amount of memory and is much faster for large sets.}
  \item{\dots}{ currently unused.}
}
\details{
looks for each element in \code{x} which elements in \code{y} are supersets  or
subsets. Note that the method can be very slow and memory intensive  if
\code{x} and/or \code{y} contain many elements.

For rules, the union of lhs and rhs is used a the set of items.
}
\value{
returns a logical matrix 
or a sparse ngCMatrix (for \code{sparse=TRUE}) 
with \code{length(x)} rows and \code{length(y)}
columns.  Each logical row vector represents which elements in \code{y} are
supersets (subsets) of the corresponding element in \code{x}.  If either
\code{x} or \code{y} have length zero, \code{NULL} is returned instead of a
matrix.
}
\seealso{
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult")
set <- eclat(Adult, parameter = list(supp = 0.8))

### find the supersets of each itemset in set
is.superset(set, set)
is.superset(set, set, sparse = FALSE)
}
\author{Michael Hahsler and Ian Johnson}
\keyword{manip}
