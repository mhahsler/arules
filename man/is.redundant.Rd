\name{is.redundant}
\alias{is.redundant}
\alias{redundant}
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
A rule is redundant if a more general rules with the same or a higher 
confidence exists.
That is, a more specific rule is redundant if it is only equally or even less 
predictive than a more general rule. A rule is more general if it has 
the same RHS but one or more items removed from the LHS. Formally,
a rule \eqn{X \Rightarrow Y}{X -> Y} is redundant if

\deqn{\exists X' \subset X \quad conf(X' \Rightarrow Y) \ge conf(X \Rightarrow Y).}{
for some X' subset X, conf(X' -> Y) >= conf(X -> Y).}

This is equivalent to a 
negative or zero \emph{improvement} as defined by Bayardo et al. (2000).
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

## mine some rules with the consequent "language in home=english"
rules <- apriori(Income, parameter = list(support = 0.5), 
  appearance = list(rhs = "language in home=english", default = "lhs"))

## for better comparison we sort the rules by confidence and add Bayado's improvement
rules <- sort(rules, by = "confidence")
quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
inspect(rules)
is.redundant(rules)

## redundant rules
inspect(rules[is.redundant(rules)])

## non-redundant rules
inspect(rules[!is.redundant(rules)])
}
\author{Michael Hahsler and Christian Buchta}
\keyword{manip}
