\name{match}
\alias{match}
\alias{\%in\%}
\alias{\%in\%,itemsets,itemsets-method}
\alias{\%in\%,itemsets,character-method}
\alias{\%pin\%}
\alias{\%oin\%}
\alias{\%ain\%}
\alias{match,itemsets,itemsets-method}
\alias{match,rules,rules-method}
\alias{match,itemMatrix,itemMatrix-method}
\title{Value Matching}
\description{
  Provides the generic function \code{match}  and the S4 methods for
  associations, transactions and itemMatrices.  \code{match} returns a vector 
  of the positions of (first) matches of its first argument in its second.

  \code{\%in\%} is a more intuitive interface as a binary operator, which
  returns a logical vector indicating if there is a match or not for
  the items in the itemsets (left operand) with the items in the table 
  (right operand).
  
  \pkg{arules} defines additional binary operators for matching itemsets:
  \code{\%pin\%} uses \emph{partial} matching on the table;
  \code{\%ain\%} itemsets have to match/include \emph{all} items in the table;
  \code{\%oin\%} itemsets can \emph{only} match/include the items in the table.
  The binary matching operators or often used in \code{\link{subset}}.
  }
\usage{
match(x, table, nomatch = NA_integer_, incomparables = NULL)

x \%in\% table
x \%pin\% table
x \%ain\% table
x \%oin\% table
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

   \code{\%in\%}, \code{\%pin\%}, \code{\%ain\%}, \code{\%oin\%}:  
   A logical vector, indicating if a match was located for each
   element of \code{x}.
   }
\seealso{
  \code{\link{subset}},
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
Adult[1:10] \%in\% "age=Middle-aged"

## find all transactions which contain items that partially match "age=" (all here).
Adult[1:10] \%pin\% "age="

## find all transactions that only include the item "age=Middle-aged" (none here).
Adult[1:10] \%oin\% "age=Middle-aged"

## find al transaction which contain both items "age=Middle-aged" and "sex=Male"
Adult[1:10] \%ain\% c("age=Middle-aged", "sex=Male")
}
\keyword{manip}
