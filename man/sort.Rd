\name{sort}
\alias{sort}
\alias{SORT}
\alias{SORT,associations-method}
\alias{sort,associations-method}
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
}
\arguments{
  \item{x}{ an object to be sorted.}
  \item{decreasing}{a logical. Should the sort be increasing or
    decreasing? (default is decreasing)}
  \item{na.last}{for controlling the treatment of \code{NAs}. If \code{TRUE},
    missing values in the data are put last; if \code{FALSE}, they are put
    first; if \code{NA}, they are removed.}
  \item{by}{ a character string specifying the quality measure stored in
    \code{x} to be used to sort \code{x}. If a vector of character strings
    is specified then the additional strings are used to sort \code{x} 
    in case of ties.}
  \item{order}{should a order vector be returned instead of the sorted 
    associations?}
  \item{...}{Further arguments are ignored.}
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

## Print the 5 rules sorted by confidence and then support as a data.frame.
as(head(sort(rules, by = c("confidence", "support")), n=5), "data.frame")

## Order of rules by decreasing support
sort(rules, by = "confidence", order = TRUE)
}
\keyword{manip}
\keyword{arith}
