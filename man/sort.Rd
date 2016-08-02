\name{sort}
\alias{sort}
\alias{SORT}
\alias{SORT,associations-method}
\alias{sort,associations-method}
\alias{head}
\alias{head,associations-method}
\alias{tail}
\alias{tail,associations-method}
\title{Sorting Associations}
\description{
  Provides the method \code{sort} to sort elements
  in class \code{\linkS4class{associations}} (e.g., itemsets or rules)
  according to the value of measures stored in the association's slot
  \code{quality} (e.g., support).
}
\usage{
\S4method{sort}{associations}(x, decreasing = TRUE, na.last = NA, 
    by = "support", order = FALSE, ...)

\S4method{head}{associations}(x, n = 6L, by = NULL, decreasing = TRUE, ...)
\S4method{tail}{associations}(x, n = 6L, by = NULL, decreasing = TRUE, ...)
}
\arguments{
  \item{x}{ an object to be sorted.}
  \item{decreasing}{a logical. Should the sort be increasing or
    decreasing? (default is decreasing)}
  \item{na.last}{ na.last is not supported for associations. 
    NAs are always put last. }
  \item{by}{ a character string specifying the quality measure stored in
    \code{x} to be used to sort \code{x}. If a vector of character strings
    is specified then the additional strings are used to sort \code{x} 
    in case of ties.}
  \item{order}{should a order vector be returned instead of the sorted 
    associations?}
  \item{n}{ a single integer indicating the number of associations returned. }
  \item{...}{Further arguments are ignored.}
}
\details{
\code{sort} is relatively slow for large sets of associations since it 
has to copy and rearrange a large data structure. 
Note that sorting creates a second copy of the set of associations which can
be slow and memory consuming for large sets.
With \code{order = TRUE} a integer vector with the order is returned instead
of the reordered associations.

If only the top \code{n} associations are needed then \code{head} using \code{by} performs this faster than calling \code{sort} and then \code{head} since 
it does it without copying and rearranging all the data. 
\code{tail} works in the same way.
}
\value{
  An object of the same class as \code{x}.
}

\seealso{
  \code{\link{associations-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

## Mine rules with APRIORI
rules <- apriori(Adult, parameter = list(supp = 0.6))

rules_by_lift <- sort(rules, by = "lift")

inspect(head(rules))
inspect(head(rules_by_lift))

## A faster/less memory consuming way to get the top 5 rules according to lift 
## (see Details section)
inspect(head(rules, n = 5, by = "lift"))
}
\keyword{manip}
\keyword{arith}
