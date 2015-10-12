\name{image}
\alias{image}
\alias{image,itemMatrix-method}
\alias{image,transactions-method}
\alias{image,tidLists-method}
\title{Visual Inspection of Binary Incidence Matrices}
\description{
  Provides the S4 methods \code{image} to generate level plots to visually
  inspect binary incidence matrices, i.e., objects based on
  \code{\linkS4class{itemMatrix}} (e.g., transactions, tidLists, items in
  itemsets or rhs/lhs in rules).  These plots can be used to identify problems
  in a data set (e.g., recording problems with some transactions containing all
  items).
}
\usage{
%image(x, \dots)
\S4method{image}{itemMatrix}(x, 
    xlab = "Items (Columns)",
    ylab = "Elements (Rows)", ...)

\S4method{image}{transactions}(x, 
    xlab = "Items (Columns)", 
    ylab = "Transactions (Rows)", ...)

\S4method{image}{tidLists}(x,
    xlab="Transactions (Columns)",
    ylab="Items/itemsets (Rows)", ...)
}
\arguments{
  \item{x}{ the object (\code{itemMatrix}, \code{transactions} or 
	\code{tidLists}). }
  \item{xlab, ylab}{labels for the plot.}
  \item{\dots}{ further arguments passed on to \code{image} in 
    package \pkg{Matrix} which in turn are passed on to
    \code{levelplot} in \pkg{lattice}.}
}
\seealso{
\code{\link[Matrix:dgTMatrix-class]{image}} (for \code{dgTMatrix} in \pkg{Matrix}),
  \code{\link[lattice]{levelplot}} (in \pkg{lattice}),
  \code{\link{itemMatrix-class}},
  \code{\link{transactions-class}},
  \code{\link{tidLists-class}}
}
\examples{
data("Epub")

## in this data set we can see that not all
## items were available from the beginning.
image(Epub[1:1000])

}
\author{Michael Hahsler}
\keyword{hplot}
