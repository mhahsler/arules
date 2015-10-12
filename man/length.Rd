\name{length}
\alias{length}
\alias{length,itemMatrix-method}
\alias{length,rules-method}
\alias{length,itemsets-method}
\alias{length,tidLists-method}
\title{Getting the Number of Elements}
\description{
  S4 methods for \code{length} which return the number of elements of
  objects defined in the package \pkg{arules}.
}
\usage{
%length(x)

\S4method{length}{rules}(x)

\S4method{length}{itemsets}(x)

\S4method{length}{tidLists}(x)

\S4method{length}{itemMatrix}(x)
}
\arguments{
  \item{x}{ an object of class 
  \code{\linkS4class{transactions}},
  \code{\linkS4class{rules}},
  \code{\linkS4class{itemsets}},
  \code{\linkS4class{tidLists}}, or
  \code{\linkS4class{itemMatrix}}.
  }
  }
\details{
  For \code{\linkS4class{itemMatrix}} and \code{\linkS4class{transactions}} the
  length is defined as the number of rows (transactions) in the binary
  incidence matrix.
  
  For sets of associations (\code{\linkS4class{rules}},
  \code{\linkS4class{itemsets}} and \code{\linkS4class{associations}} in
  general) the length is defined as the number of elements in the set (i.e.,
  the number of rules or itemsets).
  
  For \code{\linkS4class{tidLists}} the length is the number of lists (one per
  item or itemset) in the object.
}
\value{
  An integer scalar giving the \dQuote{length} of \code{x}.
}
\author{Michael Hahsler}
\keyword{attribute}
