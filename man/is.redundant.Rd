\name{is.redundant}
\alias{is.redundant}
\alias{redundant}
\alias{is.redundant,rules-method}
%
%
\title{Find Redundant Rules}
\description{
  Provides the generic function and the S4 method \code{is.redundant} 
  to find redundant rules.
}
\usage{
is.redundant(x, ...)
\S4method{is.redundant}{rules}(x, measure = "confidence", ...)
}
\arguments{
  \item{x}{ a set of rules.}
  \item{measure}{ measure used to check for redundancy. }
  \item{...}{ additional arguments. }
}
\details{
\bold{Improvement-based redundancy:}
A rule can be defined as redundant if a more general rules with the same or a higher 
confidence exists.
That is, a more specific rule is redundant if it is only equally or even less 
predictive than a more general rule. A rule is more general if it has 
the same RHS but one or more items removed from the LHS. Formally,
a rule \eqn{X \Rightarrow Y}{X -> Y} is redundant if

\deqn{\exists X' \subset X \quad conf(X' \Rightarrow Y) \ge conf(X \Rightarrow Y).}{
for some X' subset X, conf(X' -> Y) >= conf(X -> Y).}

This is equivalent to a 
negative or zero \emph{improvement} as defined by Bayardo et al. (2000).

The idea of improvement can be extended other measures besides confidence. 
Any other measure availabe for function \code{\link{interestMeasure}} (e.g., lift) can be specified in \code{measure}.

\bold{Confidence interval or the odds-ratio-based redundancy:} Li et al (2014)
propose to use the confidence interval (CI) 
of the odds ratio (OR) of rules to define redundancy. A more specific rule is 
redundant if it does not provide a significantly higher OR than any more general rule. 
Using confidence intervals as error bounds, a
more specific rule is redundant if its OR CI overlaps with the CI of any more general rule (i.e.,
the lower bound of the more specific rule's CI is lower than the upper bound of any more general rule's
CI). This type of redundancy detection is more powerful than improvement since it takes differences in counts due to randomness in the dataset into account. 

The CI of the OR can be used by specifying \code{measure = "oddsRatioCI"}.
The default used confidence level is 0.95 and can be changed with the additional parameter \code{confidenceLevel}.

The odds ratio and the CI are based on counts which can be zero and which leads to numerical problems. In addition
to the method described by Li et al (2014), we
use additive smoothing (Laplace smoothing) to alleviate this problem. The default setting adds 1 to each count.
A different pseudocount (smoothing parameter) can be defined using the additional parameter \code{smoothCounts}. Smoothing can be disabled using \code{smoothCounts = 0}.
}
\value{
returns a logical vector indicating which rules are redundant.
}
\seealso{
\code{\link{interestMeasure}}
}
\references{
Bayardo, R. , R. Agrawal, and D. Gunopulos (2000). Constraint-based rule mining in large, dense databases. \emph{Data Mining and Knowledge Discovery,} 4(2/3):217--240.

Li, J., Jixue Liu, Hannu Toivonen, Kenji Satou, Youqiang Sun, and Bingyu Sun (2014). Discovering statistically non-redundant subgroups. Knowledge-Based Systems. 67 (September, 2014), 315--327. 
\doi{10.1016/j.knosys.2014.04.030}
}
\examples{
data("Income")

## mine some rules with the consequent "language in home=english"
rules <- apriori(Income, parameter = list(support = 0.5), 
  appearance = list(rhs = "language in home=english"))

## for better comparison we add Bayado's improvement and sort by improvement
quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
rules <- sort(rules, by = "improvement")
inspect(rules)
is.redundant(rules)

## redundant rules using improvement of confidence
inspect(rules[is.redundant(rules)])

## non-redundant rules
rules_non_redundant <- rules[!is.redundant(rules)]
inspect(rules_non_redundant)
## Note: a few rules have a very small improvement over the rule {} => {language in home=english}

## use the CI of the odds ratio 
## (the first call shows how to specify count smoothing and the confidence level)
inspect(rules[is.redundant(rules, measure = "oddsRatioCI", 
  smoothCounts = 1, confidenceLevel = 0.95)])
inspect(rules[!is.redundant(rules, measure = "oddsRatioCI")])
## No rule has a significantly better OR than the simple rule {} => {language in home=english}
}
\author{Michael Hahsler and Christian Buchta}
\keyword{manip}
