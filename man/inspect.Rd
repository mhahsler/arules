\name{inspect}
\alias{inspect}
\alias{inspect,itemMatrix-method}
\alias{inspect,itemsets-method}
\alias{inspect,rules-method}
\alias{inspect,transactions-method}
\title{Display Associations and Transactions in Readable Form}
\description{
  Provides the generic function \code{inspect} and S4 methods to display
  associations and transactions plus additional information formatted for
  online inspection.
}
\usage{
inspect(x, ...)
}
\arguments{
  \item{x}{ a set of associations or transactions or an itemMatrix.}
  \item{\dots}{additional arguments can be used to customize the 
      output: \code{setStart}, \code{setEnd}, \code{itemSep} and \code{ruleSep}.
      Items are printed only one per line in case the output lines get very long.
      This can also be directly controlled using \code{linebreak}.}
}
\seealso{
  \code{\link{itemMatrix-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}},
  \code{\link{transactions-class}}
}
\author{Michael Hahsler and Kurt Hornik}
\examples{
data("Adult")
rules <- apriori(Adult)
inspect(rules[1000])

inspect(rules[1000], ruleSep = "---->", itemSep = " + ", setStart = "", setEnd ="", 
  linebreak = FALSE)
}
\keyword{print}
