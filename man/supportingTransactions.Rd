\name{supportingTransactions}
\alias{supportingTransactions}
\alias{supportingTransactions,associations-method}
\title{Supporting Transactions
}
\description{
Find transactions which support each of a set of associations and return
this information as a transaction ID list.
}
\usage{
supportingTransactions(x, transactions, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a set of associations (itemsets, rules, etc.)}
  \item{transactions}{ an object of class \code{transactions} used to mine 
  the associations in \code{x}.}
  \item{\dots}{ currently unused.}
}
\details{
The supporting transactions are all transactions of which
the itemset representing the association is a subset of.
}
\value{
An object of class \code{tidLists} containing one transaction ID list
per association in \code{x}.
}
\seealso{
 \code{\link{tidLists-class}}
}
\author{Michael Hahsler}
\examples{
data <- list(
	c("a","b","c"),
	c("a","b"),
	c("a","b","d"),
	c("b","e"),
	c("b","c","e"),
	c("a","d","e"),
	c("a","c"),
	c("a","b","d"),
	c("c","e"),
	c("a","b","d","e")
	)
data <- as(data, "transactions")

## mine itemsets
f <- eclat(data, parameter = list(support = .2, minlen=3))
inspect(f)

## find supporting Transactions
st <- supportingTransactions(f, data)
st

as(st, "list")
}
\keyword{models}
