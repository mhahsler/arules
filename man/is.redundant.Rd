\name{is.redundant}
\alias{is.redundant}
\alias{is.redundant,rules-method}
%
%
\title{Find Redundant Rules}
\description{
  Provides the generic functions and the S4 method \code{is.redundant} 
  to find redundant rules.
}
\usage{
is.redundant(x, ...)
\S4method{is.redundant}{rules}(x, measure = "confidence")
}
\arguments{
  \item{x}{ a set of rules.}
  \item{measure}{ measure used to check for redundancy. }
  \item{...}{ additional arguments. }
}
\details{
A rule is redundant if it does not improve confidence over a more specific 
rule (a rule with items added to the lhs). That is, a more specific rule is redundant if it is less 
predictive than a more general rule. This is equivalent to a 
negative \emph{improvement} as defined by Bayardo et al. (2000).

In this implementation other measures than confidence, e.g. 
improvement of lift, can be used as well. 
}
\value{
returns a logical vector indicating which rules are redundant.
}
\seealso{
\code{\link{interestMeasure}}
}
\references{
Bayardo, R. , R. Agrawal, and D. Gunopulos (2000). Constraint-based rule mining in large, dense databases. \emph{Data Mining and Knowledge Discovery,} 4(2/3):217--240.
}
\examples{
data("Income")
rules <- apriori(Income, parameter = list(support = 0.5))
is.redundant(rules)

## add Bayado's improvement for comparison
quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")

## redundant rules
inspect(rules[is.redundant(rules)])

## non-redundant rules
inspect(rules[!is.redundant(rules)])
}
\author{Michael Hahsler}
\keyword{manip}
