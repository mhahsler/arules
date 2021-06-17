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
\method{confint}{rules}(object, parm = "oddsRatio", level = 0.95, 
  smoothCounts = 0, transactions = NULL, reuse = TRUE, ...)
}
\arguments{
  \item{object}{ an object of class rules. }
  \item{parm}{ name of the interest measures (i.e., parameter). Confidence intervals are currently 
    available for the measures "support", "count", "confidence", "oddsRatio".}
  \item{level}{ the confidence level required. }
  \item{smoothCounts}{ pseudo count for addaptive smoothing (Laplace smoothing). Adds one to each count to avoids 
    counts of zero, which can lead to issues with many measures.}
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
\details{
\bold{support/count:} The confidence interval is calculared using the Poisson distribution.

\bold{confidence:} The binomial
  proportion confidence interval is calculated for confidence (Clopper and Pearson). 

\bold{lift:} Uses the confidence interval for confidence divided by the observed proportion of transactions that 
  contain Y.
  
\bold{odds ratio:} Fisher's exact confidence interval for the odds ratio is used.
  The normal approximation for the odds ratio is also available.

\bold{Normal approximations:} Approximations for the confidence intervals for the following measures
are implemented: "support_normal", "confidence_normal", and "oddsRatio_normal".

Note that these approximations
are only appropriate for large counts.

}
\value{
Returns a data.frame with one row for each rule and two columns containing the lower and upper limit of 
the confidence interval.
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

# add the odds ratio and the confidence intervals to the quality slot of the rules.
quality(rules) <- cbind(
  quality(rules), 
  oddsRatio = interestMeasure(rules, "oddsRatio"), 
  oddsRatio = ci)

rules <- sort(rules, by = "oddsRatio")
inspect(rules)

# The Nan value can be avoided by using smoothCount = 1 for interestMeasure and to avoid the Inf for
# confint.

# remove old values so they are not reused
# recalculate with additive count smoothing.
quality(rules) <- cbind(
  quality(rules), 
  oddsRatio_smoothed = interestMeasure(rules, "oddsRatio", smoothCounts = 1), 
  oddsRatio_smoothed = confint(rules, "oddsRatio", smoothCounts = 1)
  )

rules <- sort(rules, by = "oddsRatio_smoothed")
inspect(rules)
}
\keyword{manip}
