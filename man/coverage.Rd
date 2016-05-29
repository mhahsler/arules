\name{coverage}
\alias{coverage}
\alias{coverage,rules-method}
\title{Calculate coverage for rules}
\description{
   Provides the generic function and the needed S4 method to
   calculate the coverage (support of the left-hand-side) of rules.
   }
\usage{
coverage(x, transactions = NULL, reuse = TRUE)
}
\arguments{
  \item{x}{ the set of rules. }
  \item{transactions}{ the data set used to generate 'x'. Only needed if
  the quality slot of 'x' does not contain support and confidence. }
  \item{reuse}{ reuse support and confidence stored in 'x' or recompute
  from transactions? }
}
\details{
  Coverage (also called cover or LHS-support) is the support of the 
  left-hand-side of the rule, i.e., \eq{supp(X)}. It represents a measure 
  of to how often the rule can be applied.
  
  Coverage is quickly calculated from the rules quality measures 
  (support and confidence) stored in the quality slot. If these
  values are not present, then the support of the LHS is counted using 
  the data supplied in \code{transactions}.
  
  Coverage is also one of the measures available via the function
  \code{interestMeasures}.
}
\value{
   A numeric vector of the same length as \code{x} containing
   the coverage values for the sets in \code{x}.
}

\seealso{ 
\code{\link{interestMeasures}}, 
\code{\link{rules-class}} 
}
\examples{
data("Income")

## find and some rules (we only use 5 rules here) and calculate coverage
rules <- apriori(Income)[1:5]
quality(rules) <- cbind(quality(rules), coverage = coverage(rules))

inspect(rules)
}
\author{Michael Hahsler}
\keyword{models}
