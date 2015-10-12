\name{merge}
\alias{merge}
\alias{merge,itemMatrix-method}
\alias{merge,transactions-method}
\title{Merging (adding) items }
\description{
  Provides the generic function \code{merge}  and the S4 methods for
  itemMatrix and transactions. The methods are used to add new items to
  existing data.
  }
\usage{
merge(x, y, ...)
}
\arguments{
  \item{x}{ an object of class \code{itemMatrix} or \code{transactions}.}
  \item{y}{ an object of the same class as x 
  (or something which can be coerced to that class).}
  \item{...}{ further arguments; unused. }
}
\value{
    Returns a new object of the same class as x with the items in y added.
   }
\seealso{
  \code{\link{transactions-class}},
  \code{\link{itemMatrix-class}},
  \code{\link{addComplement}}
}
\author{Michael Hahsler}
\examples{
data("Groceries")

## create a random item as a matrix
randomItem <- sample(c(TRUE, FALSE), size=length(Groceries),replace=TRUE)
randomItem <- as.matrix(randomItem)
colnames(randomItem) <- "random item"
head(randomItem, 3)

## add the random item to Groceries
g2 <- merge(Groceries, randomItem)
nitems(Groceries)
nitems(g2)
inspect(head(g2,3))
}
\keyword{manip}
