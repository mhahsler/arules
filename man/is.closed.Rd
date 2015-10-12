\name{is.closed}
\alias{is.closed}
\alias{is.closed,itemsets-method}
%
%
\title{Find Closed Itemsets}
\description{
Provides the generic function and the S4 method \code{is.closed} for finding
closed itemsets.  The closure of an itemset is its largest proper superset
which has the same support (is contained in exactly the same transactions). An
itemset is closed, if it is its own closure (Pasquier et al. 1999).
}
\usage{
is.closed(x)
}
\arguments{
  \item{x}{ a set of itemsets. }
}
\value{a logical vector with the same length as \code{x} indicating
for each element in \code{x} if it is a closed itemset.
}
\seealso{
\code{\link{itemsets-class}}
}
\references{
Nicolas Pasquier, Yves Bastide, Rafik Taouil, and Lotfi Lakhal (1999).
Discovering frequent closed itemsets for association rules. In \emph{Proceeding
of the 7th International Conference on Database Theory}, Lecture Notes In
Computer Science (LNCS 1540), pages 398--416. Springer, 1999.
}
\author{Michael Hahsler}
\keyword{models}
