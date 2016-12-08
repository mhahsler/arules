\name{duplicated}
\alias{duplicated}
\alias{duplicated,itemsets-method}
\alias{duplicated,rules-method}
\alias{duplicated,itemMatrix-method}
\title{Find Duplicated Elements}
\description{
  Provides the generic function \code{duplicated}  and the S4 methods for
  \code{itemMatrix} and associations.  \code{duplicated} finds duplicated
  elements in an \code{itemMatrix}.  It returns a logical vector indicating
  which elements are duplicates.
  
  Note that \code{duplicated} can also be used to find transactions with
  identical items and identical rules and itemsets stored in
  \code{rules} and \code{itemsets}.
}
\usage{
duplicated(x, incomparables = FALSE, ...)
}
\arguments{
  \item{x}{ an object of class \code{itemMatrix} or associations.}
  \item{\dots}{ further arguments (currently unused). }
  \item{incomparables}{ argument currently unused.}
}
\value{
   A logical vector indicating duplicated elements.
}
\seealso{
  \code{\link{unique}},
  \code{\link{rules-class}},
  \code{\link{itemsets-class}},
  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult")

r1 <- apriori(Adult[1:1000], parameter = list(support = 0.5))
r2 <- apriori(Adult[1001:2000], parameter = list(support = 0.5))

## Note this creates a collection of rules from two sets of rules
r_comb <- c(r1, r2)
duplicated(r_comb)

}
\author{Michael Hahsler}
\keyword{manip}
