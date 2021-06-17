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

\S4method{is.redundant}{rules}(x, measure = "confidence", 
  confint = FALSE, level = 0.95, smoothCounts = 1, ...)
}
\arguments{
  \item{x}{ a set of rules.}
  \item{measure}{ measure used to check for redundancy. }
  \item{confint}{ should confidence intervals be used to the redundancy check? }
  \item{level}{ confidence level for the confidence interval. Only used when \code{confint = TRUE}. }
  \item{smoothCounts}{adds a "pseudo count" to each count in the used contingency table. This implements
  addaptive smoothing (Laplace smoothing) for counts and avoids zero counts.}
  \item{...}{ additional arguments are passed on to \code{\link{interestMeasure}}, or, for
    \code{confint = TRUE} to \code{\link{confint}}. }
}
\details{
\bold{Simple improvement-based redundancy:} (\code{confint = FALSE})
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
Any other measure availabe for function \code{\link{interestMeasure}} (e.g., lift or the odds ratio) 
can be specified in \code{measure}.

\bold{Confidence interval-based redundancy:}  (\code{confint = TRUE})
Li et al (2014) propose to use the confidence interval (CI) 
of the odds ratio (OR) of rules to define redundancy. A more specific rule is 
redundant if it does not provide a significantly higher OR than any more general rule. 
Using confidence intervals as error bounds, a
more specific rule is redundant if its OR CI overlaps with the CI of any more general rule (i.e.,
the lower bound of the more specific rule's CI is lower than the upper bound of any more general rule's
CI). This type of redundancy detection is more powerful than improvement since it takes differences in counts due to randomness in the dataset into account. 

The odds ratio and the CI are based on counts which can be zero and which leads to numerical problems. In addition
to the method described by Li et al (2014), we
use additive smoothing (Laplace smoothing) to alleviate this problem. The default setting adds 1 to each count (see
  \code{\link{confint}}).
A different pseudocount (smoothing parameter) can be defined using the additional parameter \code{smoothCounts}. Smoothing can be disabled using \code{smoothCounts = 0}.

Confidence interval-based redundancy checks can also be used for other measures with a confidence interval
like confidence (see \code{\link{confint}}).
}
\value{
returns a logical vector indicating which rules are redundant.
}
\seealso{
\code{\link{interestMeasure}},
\code{\link{confint}}
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

## find non-redundant rules using improvement of confidence 
## Note: a few rules have a very small improvement over the rule {} => {language in home=english}
rules_non_redundant <- rules[!is.redundant(rules)]
inspect(rules_non_redundant)

## use non-overlapping confidence intervals for the confidence measure instead
## Note: a fewer rules have a significantly higher confidence
inspect(rules[!is.redundant(rules, measure = "confidence", 
  confint = TRUE, level = 0.95)])

## find non-redundant rules using improvement of the odds ratio.
quality(rules)$oddsRatio <-  interestMeasure(rules, measure = "oddsRatio", smoothCounts = 1)
inspect(rules[!is.redundant(rules, measure = "oddsRatio")])

## use the confidence interval for the odds ratio. 
## We see that no rule has a significantly better odds ratio than the most general rule.
inspect(rules[!is.redundant(rules, measure = "oddsRatio", 
  confint = TRUE, level = 0.95)])
}
\author{Michael Hahsler and Christian Buchta}
\keyword{manip}
