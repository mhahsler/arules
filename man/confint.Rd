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
\method{confint}{rules}(object, parm, level = 0.95, 
  smoothCounts = 1, transactions = NULL, reuse = TRUE, ...)
}
\arguments{
  \item{object}{ an object of class rules. }
  \item{parm}{ name of the interest measures (i.e., parameter). Confidence intervals are currently 
    available for the measure "oddsRatio".}
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
The normal approximation for the odds Ratio is described in Li et al 2014.
}
\value{
Returns a data.frame with one row for each rule and two columns containing the lower and upper bound of 
the confidence interval.
}
\references{
Li, J., Jixue Liu, Hannu Toivonen, Kenji Satou, Youqiang Sun, and Bingyu Sun (2014). Discovering statistically non-redundant subgroups. Knowledge-Based Systems. 67 (September, 2014), 315--327. 
\doi{10.1016/j.knosys.2014.04.030}
}
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

# add the odds ratio and the confidence intervals to the quality slot of the rules. Note that
# I use count smoothing to calculate the odds ratio. confint uses count smoothing by default. 
quality(rules) <- cbind(
  quality(rules), 
  oddsRatio = interestMeasure(rules, "oddsRatio", smoothCount = 1), 
  oddsRatio = ci)

inspect(rules)
}
\keyword{manip}
