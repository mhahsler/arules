\name{confint}
\alias{confint}
\alias{confint.rules}
\title{
Confidence Intervals for Association Interest Measures
}
\description{
Computes confidence intervals for interest Measures used in association rule mining.
}
\usage{
\method{confint}{rules}(object, parm = "oddsRatio", level = 0.95, measure = NULL, 
  method = "wald", smoothCounts = 0, transactions = NULL, reuse = TRUE, ...)
}
\arguments{
  \item{object}{ an object of class rules. }
  \item{parm, measure}{ name of the interest measures (i.e., parameter). Confidence intervals are currently 
    available for the measures 
    "support", 
    "count", 
    "confidence",
    "kappa",
    "lift",
    "oddsRatio", and
    "phi".
    \code{measure} can be used instead of \code{parm}.}
  \item{level}{ the confidence level required. }
  \item{method}{ method to calculate the confidence interval. Typically available are "exact" (binomial proportion, Fisher's exact confidence interval) and "wald" (adjusted Wald interval).}
  \item{smoothCounts}{ pseudo count for addaptive smoothing (Laplace smoothing). Note that this count smoothing is 
    performed in addition to the corrections already included when calculating the confidence interval. }
   \item{transactions}{ the transaction data set used to mine
    the associations or a set of different transactions to calculate
    the confidence interval for (Note: you need to set \code{reuse = FALSE} in the
    later case). }
  \item{reuse}{ logical indicating if information in quality slot should
      be reuse for calculating the measures. 
      Use \code{reuse = FALSE} to force counting (might be very slow but
      is necessary if you use a different set of transactions than was used
      for mining).}
    \item{...}{Additional parameters are ignored with a warning.}
}
%'
%\details{
%}
\value{
Returns a data.frame with one row for each rule and the two columns "lowerLimit" and "upperLimit" with 
the interval. The data frame has the attributes:

\item{measure}{ the interest measure. }
\item{level}{ the confidence level}
\item{method}{ desciption of the used method to calculate the confidence interval. }
}
%\references{
%}
\author{Michael Hahsler}
\seealso{
\code{\link{interestMeasure}},
\code{\link{is.redundant}}
}
\examples{
data("Income")

# mine some rules with the consequent "language in home=english"
rules <- apriori(Income, parameter = list(support = 0.5), 
  appearance = list(rhs = "language in home=english"))
 
# calculate the confidence interval 
ci <- confint(rules, "oddsRatio")
ci
attributes(ci)


# add the odds ratio (with smoothCounts = .5 to perform Haldane-Anscombe correction) 
# and the confidence intervals (which are by default corrected) to the quality slot of the rules.
quality(rules) <- cbind(
  quality(rules), 
  oddsRatio = interestMeasure(rules, "oddsRatio", smoothCounts = .5), 
  oddsRatio = ci)

rules <- sort(rules, by = "oddsRatio")
inspect(rules)

# use confidence intervals for lift to find rules with a lift significantly larger then 1. 
# The confidence level is decreased to 0.9 to have 5% above the upper limit and we check
# if the interval does not cover 1 (i.e., the lower limit is larger than 1).
ci <- confint(rules, "lift", level = 0.9)
ci

inspect(rules[ci[, "lowerLimit"] > 1])
}
\keyword{manip}
