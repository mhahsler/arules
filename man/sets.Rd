\name{setOperations}
\alias{setOperations}
\alias{sets}
\alias{union}
\alias{union-methods}
%\alias{union,associations,associations-method}
\alias{union,associations-method}
%\alias{union,itemMatrix,itemMatrix-method}
\alias{union,itemMatrix-method}
\alias{intersect}
\alias{intersect-methods}
\alias{intersect,associations-method}
%\alias{intersect,associations,associations-method}
%\alias{intersect,itemMatrix,itemMatrix-method}
\alias{intersect,itemMatrix-method}
\alias{setequal}
\alias{setequal-methods}
%\alias{setequal,associations,associations-method}
\alias{setequal,associations-method}
%\alias{setequal,itemMatrix,itemMatrix-method}
\alias{setequal,itemMatrix-method}
\alias{setdiff}
\alias{setdiff-methods}
%\alias{setdiff,associations,associations-method}
\alias{setdiff,associations-method}
%\alias{setdiff,itemMatrix,itemMatrix-method}
\alias{setdiff,itemMatrix-method}
\alias{is.element}
\alias{is.element-methods}
%\alias{is.element,associations,associations-method}
\alias{is.element,associations-method}
%\alias{is.element,itemMatrix,itemMatrix-method}
\alias{is.element,itemMatrix-method}
%
%
\title{Set Operations}
\description{
  Provides the generic functions and the S4 methods for the set operations
  \code{union},  \code{intersect}, \code{setequal},  \code{setdiff} and
  \code{is.element} on sets of associations (e.g., rules, itemsets) and
  itemMatrix. 
}
\usage{
union(x, y, ...)
intersect(x, y, ...)
setequal(x, y, ...)
setdiff(x, y, ...)
is.element(el, set, ...)
}
\arguments{
  \item{x,y, el, set}{ sets of associations or itemMatrix objects.}
  \item{...}{ Other arguments are unused.}
}
\details{
All S4 methods for set operations are defined for the 
class name \code{"ANY"} in the signature, 
so they should work for all S4 classes for which the following
methods are available: \code{match}, \code{length} and \code{unique}. }
\value{
\code{union}, \code{intersect}, \code{setequal} and \code{setdiff} return an
object of the same class as \code{x} and \code{y}.

\code{is.element} returns a logic vector of length \code{el} indicating
for each element if it is included in \code{set}.
}
\seealso{
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

## mine some rules
r <- apriori(Adult)

## take 2 subsets
r1 <- r[1:10]
r2 <- r[6:15]

union(r1,r2)
intersect(r1,r2)
setequal(r1,r2)
}
\keyword{manip}
