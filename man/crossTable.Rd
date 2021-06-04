\name{crossTable}
\alias{crossTable}
\alias{crossTable,itemMatrix-method}
\title{Cross-tabulate joint occurrences across pairs of items}
\description{
  Provides the generic function \code{crossTable} and the S4 method to 
  cross-tabulate joint occurrences across all pairs of items.
}
\usage{
crossTable(x, ...) 

\S4method{crossTable}{itemMatrix}(x, measure = c("count", "support", "probability", "lift"), 
  sort = FALSE)
}
\arguments{
  \item{x}{ object to be cross-tabulated 
    (\code{transactions} or \code{itemMatrix}).}
  \item{measure}{ measure to return. Default is co-occurrence counts. } 
  \item{sort}{ sort the items by support. } 
  \item{...}{ additional arguments. } 
}
\value{
  A symmetric matrix of n time n, where n is the number of items times 
  in \code{x}. The matrix contains the co-occurrence counts between pairs of
  items.
}
\seealso{
  \code{\link{transactions-class}},
  \code{\link{itemMatrix-class}}.
}
\examples{
data("Groceries")

ct <- crossTable(Groceries, sort=TRUE)
ct[1:5, 1:5]

sp <- crossTable(Groceries, measure="support", sort=TRUE)
sp[1:5,1:5]

lift <- crossTable(Groceries, measure="lift", sort=TRUE)
lift[1:5,1:5]
}
\author{Michael Hahsler}
\keyword{models}
