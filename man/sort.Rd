\name{sort}
\alias{sort}
\alias{SORT}
\alias{SORT,associations-method}
\alias{sort,associations-method}
\alias{head}
\alias{head,associations-method}
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
has copy and rearrange a large data structure. 
With \code{order = TRUE} a integer vector with the order is returned instead
of the reordered associations.

If only the top n associations
are needed then \code{head} using \code{by} performs this faster 
(without copying and rearranging all the data). 
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

## Fast way to get the top 3 rules according to confidence
inspect(head(rules, n = 3, by = "confidence"))

## Order of rules by decreasing support
sort(rules, by = "confidence", order = TRUE)
}
\keyword{manip}
\keyword{arith}
