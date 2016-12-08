\name{unique}
\alias{unique}
\alias{unique,associations-method}
\alias{unique,itemMatrix-method}
\title{Remove Duplicated Elements from a Collection} 
\description{
  Provides the generic function \code{unique}   and the S4 methods for
  \code{itemMatrix}.  \code{unique} uses \code{\link{duplicated}} to return an
  \code{itemMatrix} with the duplicate elements removed.
  
  Note that \code{unique} can also be used on collections of 
  \code{associations}.
  }
\usage{
unique(x, incomparables = FALSE, ...)
}
\arguments{
  \item{x}{ an object of class \code{itemMatrix} or associations. }
  \item{\dots}{ further arguments (currently unused). }
  \item{incomparables}{ currently unused.}
}
\value{
  An object of the same class as \code{x} with duplicated elements removed.
}
\seealso{
  \code{\link{duplicated}},
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data("Adult")

r1 <- apriori(Adult[1:1000], parameter = list(support = 0.5))
r2 <- apriori(Adult[1001:2000], parameter = list(support = 0.5))

## Note that this produces a collection of rules from two sets
r_comb <- c(r1, r2)
r_comb <- unique(r_comb)
r_comb

}
\keyword{manip}
