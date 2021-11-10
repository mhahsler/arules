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
  measure = NULL, sides = c("two", "one"), method = NULL, replications = 1000,
  smoothCounts = 0, transactions = NULL, ...)
}
\arguments{
  \item{object}{ an object of class rules. }
  \item{parm, measure}{ name of the interest measures (i.e., parameter). Confidence intervals are currently 
    available for the measures 
    "support", 
    "count", 
    "confidence",
    "lift",
    "oddsRatio", and
    "phi".
    \code{measure} can be used instead of \code{parm}.}
  \item{level}{ the confidence level required. }
  \item{sides}{ Should a two-sided confidence interval or two one-sided limits be returned?}
  \item{method}{ method to construct the confidence interval. The available methods depends on the 
    measure and the most common method is used by default. }
  \item{smoothCounts}{ pseudo count for addaptive smoothing (Laplace smoothing). Often a pseudo counts
  of .5 is used for smoothing (see Detail Section). }
  \item{replications}{ number of replications for method "simulation". Ignored for other methods.}
  \item{transactions}{ if the rules object does not contain sufficient quality information, then a set
    of transactions to calculate the confidence interval for can be specified. }
  \item{...}{Additional parameters are ignored with a warning.}
}
\details{
This method creates a contingency table for each rule and then constructs a confidence intervals
for the specified measures.

Haldan-Anscombe correction (Haldan, 1940; Anscombe, 1956) is \code{smoothCounts = 0.5} where
  .5 is added to each count in the contingency table. This avoids issues with zero counts.
}
\value{
Returns a matrix with with one row for each rule and the two columns "LL" and "UL" with 
the interval. The matrix has the additional attributes:

\item{measure}{ the interest measure.}
\item{level}{ the confidence level}
\item{sides}{ the confidence level}
\item{smoothCounts}{ used count smoothing. }
\item{method}{ name of the method to create the interval }
\item{desc}{ desciption of the used method to calculate the confidence interval. The mentioned references can be found below. }
}
\references{
Wilson, E. B. (1927). "Probable inference, the law of succession, and statistical inference". Journal of the American Statistical Association. 22 (158): 209-212. \doi{10.1080/01621459.1927.10502953}

Clopper, C.; Pearson, E. S. (1934). "The use of confidence or fiducial limits illustrated in the case of the binomial". Biometrika. 26 (4): 404-413. \doi{10.1093/biomet/26.4.404}

Doob, J. L. (1935). "The Limiting Distributions of Certain Statistics". Annals of Mathematical Statistics. 6: 160-169. \doi{10.1214/aoms/1177732594}

Fisher, R.A. (1962). "Confidence limits for a cross-product ratio". Australian Journal of Statistics, 4, 41.

Woolf, B. (1955). "On estimating the relation between blood group and diseases". Annals of Human Genetics, 19, 251-253.

Haldane, J.B.S. (1940). "The mean and variance of the moments of chi-squared when used as a test of homogeneity, when expectations are small". Biometrika, 29, 133-134.

Anscombe, F.J. (1956). "On estimating binomial response relations". Biometrika, 43, 461-464.
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

inspect(rules[ci[, "LL"] > 1])
}
\keyword{manip}
