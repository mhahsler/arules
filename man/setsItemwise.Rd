\name{itemSetOperations}
\alias{itemSetOperations}
\alias{itemUnion}
\alias{itemUnion,itemMatrix,itemMatrix-method}
\alias{itemSetdiff}
\alias{itemSetdiff,itemMatrix,itemMatrix-method}
\alias{itemIntersect}
\alias{itemIntersect,itemMatrix,itemMatrix-method}

\title{Itemwise Set Operations}
\description{
  Provides the generic functions and the S4 methods for itemwise
  set operations on items in an itemMatrix. The regular set operations regard each itemset in an itemMatrix as an element. Itemwise operations regard each item as an element
   and operate on the items of pairs if corresponding itemsets (first itemset in x with first itemset in y, second with second, etc.).}
\usage{
itemUnion(x, y)
itemSetdiff(x, y)
itemIntersect(x, y)
}
\arguments{
    \item{x,y}{ two itemMatrix objects with the
    same number of rows (itemsets). }
}
\value{
  An object
   of class \code{itemMatrix} is returned.
}
\seealso{  
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

fsets <- eclat(Adult, parameter = list(supp = 0.5))
inspect(fsets[1:4])
inspect(itemUnion(items(fsets[1:2]), items(fsets[3:4])))
inspect(itemSetdiff(items(fsets[1:2]), items(fsets[3:4])))
inspect(itemIntersect(items(fsets[1:2]), items(fsets[3:4])))
}
\keyword{manip}
