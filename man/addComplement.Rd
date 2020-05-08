\name{addComplement}
\alias{addComplement}
\alias{addComplement,transactions-method}
\title{Add Complement-items to Transactions}
\description{
  Provides the generic function \code{addComplement} and the S4 methods for
  transactions. This function adds for given items complement items. That is
  it adds an artificial item to each transactions which does not
  contain the original item. Such items are also called negative items (Antonie et al, 2014).
  }
\usage{
addComplement(x, labels, complementLabels=NULL)
}
\arguments{
  \item{x}{ an object of class \code{transactions}.}
  \item{labels}{ character strings; item labels for which complements 
    should be created.}
  \item{complementLabels}{ character strings; labels for the 
    artificial complement-items. If omitted then the original label
    is prepended by "!" to form the complement-item label.}
}
\value{
   Returns an object of class \code{transactions} with complement-items added.
   }
\seealso{
  \code{\link{transactions-class}},
  \code{\link{merge}}
}
\references{
Antonie L., Li J., Zaiane O. (2014) Negative Association Rules. In: Aggarwal C., Han J. (eds) \emph{Frequent Pattern Mining,} Springer International Publishing, pp. 135-145. \doi{10.1007/978-3-319-07821-2_6}
}
\author{Michael Hahsler}

\examples{
data("Groceries")

## add a complement-items for "whole milk" and "other vegetables"
g2 <- addComplement(Groceries, c("whole milk", "other vegetables"))
g2
tail(itemInfo(g2))
inspect(head(g2, 3))

## use a custom label for the complement-item
g2 <- addComplement(g2, "coffee", complementLabels = "NO coffee")
inspect(head(g2, 3))
}
\keyword{manip}
