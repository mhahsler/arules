\name{combine}
\alias{c}
\alias{c,itemMatrix-method}
\alias{c,transactions-method}
\alias{c,rules-method}
\alias{c,itemsets-method}
\title{Combining Objects}
\description{
  Provides the S4 methods to combine several objects based on
  \code{\linkS4class{itemMatrix}} into a single object.
  
  Note, use \code{\link{union}} rather than \code{c} to combine several mined
  \code{\linkS4class{itemsets}} (or \code{\linkS4class{rules}}) into a single
  set.
}
\usage{
%c(x, \ldots, recursive = FALSE)
\S4method{c}{itemMatrix}(x, ..., recursive = FALSE)

\S4method{c}{transactions}(x, ..., recursive = FALSE)

\S4method{c}{rules}(x, ..., recursive = FALSE)

\S4method{c}{itemsets}(x, ..., recursive = FALSE)
}
\arguments{
  \item{x}{ first object.}
  \item{\dots}{ further objects of the same class as \code{x} to be combined.}
  \item{recursive}{ a logical. If \code{recursive=TRUE}, 
    the function recursively
    descends through lists combining all their elements into a
    vector.}
}
\value{
  An object of the same class as \code{x}.
}
\seealso{
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}},
  \code{\link{rules-class}},
  \code{\link{itemsets-class}}
}
\examples{
data("Adult")

## combine transactions
a1 <- Adult[1:10]
a2 <- Adult[101:110]

aComb <- c(a1, a2)
summary(aComb)

## combine rules (can contain the same rule multiple times)
r1 <- apriori(Adult[1:1000])
r2 <- apriori(Adult[1001:2000])
rComb <- c(r1, r2) 
rComb

## union of rules (a set with only unique rules: same as unique(rComb))
rUnion <- union(r1,r2)
rUnion
}
\author{Michael Hahsler}
\keyword{manip}
