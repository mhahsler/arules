\name{match}
\alias{match}
\alias{\%in\%}
\alias{match,itemsets,itemsets-method}
\alias{match,rules,rules-method}
\alias{match,itemMatrix,itemMatrix-method}
\title{Value Matching}
\description{
  Provides the generic function \code{match}  and the S4 methods for
  associations and transactions.  \code{match} returns a vector of the
  positions of (first) matches of its first argument in its second.

  \code{\%in\%} is a more intuitive interface as a binary operator, which
  returns a logical vector indicating if there is a match or not for
  its left operand.
  }
\usage{
match(x,  table, nomatch = NA_integer_, incomparables = NULL)

x \%in\% table
}
\arguments{
  \item{x}{ an object of class \code{itemMatrix}, \code{transactions} or 
    \code{associations}.}
  \item{table}{ a set of associations or transactions to be matched against.}
  \item{nomatch}{ the value to be returned in the case when no match is found. }
  \item{incomparables}{ not implemented. }
}
\value{
   \code{match}: An integer vector of the same length as \code{x}  giving the
   position in \code{table} of the first match if there is a match, otherwise
   \code{nomatch}.

   \code{\%in\%}:  A logical vector, indicating if a match was located for each
   element of \code{x}.
   }
\seealso{
  \code{\link{rules-class}},
  \code{\link{itemsets-class}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

## get unique transactions, count frequency of unique transactions 
## and plot frequency of unique transactions
vals <- unique(Adult)
cnts <- tabulate(match(Adult, vals))
plot(sort(cnts, decreasing=TRUE))

## find all transactions which are equal to transaction 10 in Adult
which(Adult \%in\% Adult[10])

## for transactions we can also match directly with itemLabels.
## Find in the first 10 transactions the ones which 
## contain age=Middle-aged (see help page for class itemMatrix)
Adult[1:10] %in% "age=Middle-aged"
}
\keyword{manip}
