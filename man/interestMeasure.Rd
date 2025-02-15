% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interestMeasures.R
\name{interestMeasure}
\alias{interestMeasure}
\alias{interestMeasure,itemsets-method}
\alias{interestMeasure,rules-method}
\title{Calculate Additional Interest Measures}
\usage{
interestMeasure(x, measure, transactions = NULL, reuse = TRUE, ...)

\S4method{interestMeasure}{itemsets}(x, measure, transactions = NULL, reuse = TRUE, ...)

\S4method{interestMeasure}{rules}(x, measure, transactions = NULL, reuse = TRUE, ...)
}
\arguments{
\item{x}{a set of \link{itemsets} or \link{rules}.}

\item{measure}{name or vector of names of the desired interest measures (see
the Details section for available measures). If measure is missing then all available
measures are calculated.}

\item{transactions}{the \link{transactions} used to mine the associations
or a set of different transactions to calculate interest measures from
(Note: you need to set \code{reuse = FALSE} in the later case).}

\item{reuse}{logical indicating if information in the quality slot should be
reuse for calculating the measures. This speeds up the process significantly
since only very little (or no) transaction counting is necessary if support,
confidence and lift are already available. Use \code{reuse = FALSE} to force
counting (might be very slow but is necessary if you use a different set of
transactions than was used for mining).}

\item{...}{further arguments for the measure calculation. Many measures
are based on contingency table counts and zero counts can produce \code{NaN} values
(division by zero). This issue can be resolved by using the additional
parameter \code{smoothCounts} which performs additive smoothing by adds a
"pseudo count" of \code{smoothCounts} to each cell in the contingency
table. Use \code{smoothCounts = 1} or larger values for Laplace smoothing.
Use \code{smoothCounts = .5} for Haldane-Anscombe correction (Haldan, 1940; Anscombe, 1956)
which is often used for chi-squared, phi correlation and related measures.}
}
\value{
If only one measure is used, the function returns a numeric vector
containing the values of the interest measure for each association in the
set of associations \code{x}.

If more than one measures are specified, the result is a data.frame
containing the different measures for each association as columns.

\code{NA} is returned for rules/itemsets for which a certain measure is not
defined.
}
\description{
Provides the generic function \code{interestMeasure()} and the
methods to calculate various additional interest measures for existing sets
of \link{itemsets} or \link{rules}.
}
\details{
A searchable list of definitions, equations and
references for all available interest measures can be found at
\url{https://mhahsler.github.io/arules/docs/measures}. The descriptions
are also linked in the list below.

The following measures are implemented for \strong{itemsets}:
\itemize{
\item "support": \href{https://mhahsler.github.io/arules/docs/measures#support}{Support.}
\item "count": \href{https://mhahsler.github.io/arules/docs/measures#count}{Support Count.}
\item "allConfidence": \href{https://mhahsler.github.io/arules/docs/measures#allconfidence}{All-Confidence.}
\item "crossSupportRatio": \href{https://mhahsler.github.io/arules/docs/measures#crosssupportratio}{Cross-Support Ratio.}
\item "lift": \href{https://mhahsler.github.io/arules/docs/measures#lift}{Lift.}
}

The following measures are implemented for \strong{rules}:
\itemize{
\item "support": \href{https://mhahsler.github.io/arules/docs/measures#support}{Support.}
\item "confidence": \href{https://mhahsler.github.io/arules/docs/measures#confidence}{Confidence.}
\item "lift": \href{https://mhahsler.github.io/arules/docs/measures#lift}{Lift.}
\item "count": \href{https://mhahsler.github.io/arules/docs/measures#count}{Support Count.}
\item "addedValue": \href{https://mhahsler.github.io/arules/docs/measures#addedvalue}{Added Value.}
\item "boost": \href{https://mhahsler.github.io/arules/docs/measures#boost}{Confidence Boost.}
\item "casualConfidence": \href{https://mhahsler.github.io/arules/docs/measures#casualconfidence}{Casual Confidence.}
\item "casualSupport": \href{https://mhahsler.github.io/arules/docs/measures#casualsupport}{Casual Support.}
\item "centeredConfidence": \href{https://mhahsler.github.io/arules/docs/measures#centeredconfidence}{Centered Confidence.}
\item "certainty": \href{https://mhahsler.github.io/arules/docs/measures#certainty}{Certainty Factor.}
\item "chiSquared": \href{https://mhahsler.github.io/arules/docs/measures#chisquared}{Chi-Squared.} Additional parameters are: \code{significance = TRUE} returns the p-value of the test for independence instead of the chi-squared statistic. For p-values, substitution effects (the occurrence of one item makes the occurrence of another item less likely) can be tested using the parameter \code{complements = FALSE}.  Note: Correction for multiple comparisons can be done using \code{\link[stats:p.adjust]{stats::p.adjust()}}.
\item "collectiveStrength": \href{https://mhahsler.github.io/arules/docs/measures#collectivestrength}{Collective Strength.}
\item "confirmedConfidence": \href{https://mhahsler.github.io/arules/docs/measures#confirmedconfidence}{Descriptive Confirmed Confidence.}
\item "conviction": \href{https://mhahsler.github.io/arules/docs/measures#conviction}{Conviction.}
\item "cosine": \href{https://mhahsler.github.io/arules/docs/measures#cosine}{Cosine.}
\item "counterexample": \href{https://mhahsler.github.io/arules/docs/measures#counterexample}{Example and Counter-Example Rate.}
\item "coverage": \href{https://mhahsler.github.io/arules/docs/measures#coverage}{Coverage.}
\item "doc": \href{https://mhahsler.github.io/arules/docs/measures#doc}{Difference of Confidence.}
\item "fishersExactTest": \href{https://mhahsler.github.io/arules/docs/measures#fishersexacttest}{Fisher's Exact Test.} By default complementary effects are mined, substitutes can be found by using the parameter \code{complements = FALSE}. Note that Fisher's exact test is equal to hyper-confidence with \code{significance = TRUE}. Correction for multiple comparisons can be done using \code{\link[stats:p.adjust]{stats::p.adjust()}}.
\item "gini": \href{https://mhahsler.github.io/arules/docs/measures#gini}{Gini Index.}
\item "hyperConfidence": \href{https://mhahsler.github.io/arules/docs/measures#hyperconfidence}{Hyper-Confidence.} Reports the confidence level by default and the significance level if \code{significance = TRUE} is used. By default complementary effects are mined, substitutes (too low co-occurrence counts) can be found by using the parameter \code{complements = FALSE}.
\item "hyperLift": \href{https://mhahsler.github.io/arules/docs/measures#hyperlift}{Hyper-Lift.} The used quantile can be changed using parameter \code{level} (default: \code{level = 0.99}).
\item "imbalance": \href{https://mhahsler.github.io/arules/docs/measures#imbalance}{Imbalance Ratio.}
\item "implicationIndex": \href{https://mhahsler.github.io/arules/docs/measures#implicationindex}{Implication Index.}
\item "importance": \href{https://mhahsler.github.io/arules/docs/measures#importance}{Importance.}
\item "improvement": \href{https://mhahsler.github.io/arules/docs/measures#improvement}{Improvement.} The additional parameter \code{improvementMeasure} (default: \code{'confidence'}) can be used to specify the measure used for the improvement calculation. See \href{https://mhahsler.github.io/arules/docs/measures#generalizedImprovement}{Generalized improvement}.
\item "jaccard": \href{https://mhahsler.github.io/arules/docs/measures#jaccard}{Jaccard Coefficient.}
\item "jMeasure": \href{https://mhahsler.github.io/arules/docs/measures#jmeasure}{J-Measure.}
\item "kappa": \href{https://mhahsler.github.io/arules/docs/measures#kappa}{Kappa.}
\item "kulczynski": \href{https://mhahsler.github.io/arules/docs/measures#kulczynski}{Kulczynski.}
\item "lambda": \href{https://mhahsler.github.io/arules/docs/measures#lambda}{Lambda.}
\item "laplace": \href{https://mhahsler.github.io/arules/docs/measures#laplace}{Laplace Corrected Confidence.} Parameter \code{k} can be used to specify the number of classes (default is 2).
\item "leastContradiction": \href{https://mhahsler.github.io/arules/docs/measures#leastcontradiction}{Least Contradiction.}
\item "lerman": \href{https://mhahsler.github.io/arules/docs/measures#lerman}{Lerman Similarity.}
\item "leverage": \href{https://mhahsler.github.io/arules/docs/measures#leverage}{Leverage.}
\item "LIC": \href{https://mhahsler.github.io/arules/docs/measures#lic}{Lift Increase.} The additional parameter \code{improvementMeasure} (default: \code{'lift'}) can be used to specify the measure used for the increase calculation.  See \href{https://mhahsler.github.io/arules/docs/measures#ginc}{Generalized increase ratio}.
\item "maxconfidence": \href{https://mhahsler.github.io/arules/docs/measures#maxconfidence}{MaxConfidence.}
\item "mutualInformation": \href{https://mhahsler.github.io/arules/docs/measures#mutualinformation}{Mutual Information.}
\item "oddsRatio": \href{https://mhahsler.github.io/arules/docs/measures#oddsratio}{Odds Ratio.}
\item "phi": \href{https://mhahsler.github.io/arules/docs/measures#phi}{Phi Correlation Coefficient.}
\item "ralambondrainy": \href{https://mhahsler.github.io/arules/docs/measures#ralambondrainy}{Ralambondrainy.}
\item "relativeRisk": \href{https://mhahsler.github.io/arules/docs/measures#relativerisk}{Relative Risk.}
\item "rhsSupport": \href{https://mhahsler.github.io/arules/docs/measures#rhssupport}{Right-Hand-Side Support.}
\item "RLD": \href{https://mhahsler.github.io/arules/docs/measures#rld}{Relative Linkage Disequilibrium.}
\item "rulePowerFactor": \href{https://mhahsler.github.io/arules/docs/measures#rulepowerfactor}{Rule Power Factor.}
\item "sebag": \href{https://mhahsler.github.io/arules/docs/measures#sebag}{Sebag-Schoenauer.}
\item "stdLift": \href{https://mhahsler.github.io/arules/docs/measures#stdlift}{Standardized Lift.}
\item "table": \href{https://mhahsler.github.io/arules/docs/measures#table}{Contingency Table.} Returns the four counts for the contingency table. The entries are labeled \code{n11}, \code{n01}, \code{n10}, and \code{n00} (the first subscript is for X and the second is for Y; 1 indicated presence and 0 indicates absence). If several measures are specified, then the counts have the prefix \code{table.}
\item "varyingLiaison": \href{https://mhahsler.github.io/arules/docs/measures#varyingliaison}{Varying Rates Liaison.}
\item "yuleQ": \href{https://mhahsler.github.io/arules/docs/measures#yuleq}{Yule's Q.}
\item "yuleY": \href{https://mhahsler.github.io/arules/docs/measures#yuley}{Yule's Y.}
}
}
\examples{

data("Income")
rules <- apriori(Income)

## calculate a single measure and add it to the quality slot
quality(rules) <- cbind(quality(rules),
  hyperConfidence = interestMeasure(rules,
    measure = "hyperConfidence",
    transactions = Income
  )
)

inspect(head(rules, by = "hyperConfidence"))

## calculate several measures
m <- interestMeasure(rules, c("confidence", "oddsRatio", "leverage"),
  transactions = Income
)
inspect(head(rules))
head(m)

## calculate all available measures for the first 5 rules and show them as a
## table with the measures as rows
t(interestMeasure(head(rules, 5), transactions = Income))

## calculate measures on a different set of transactions (I use a sample here)
## Note: reuse = TRUE (default) would just return the stored support on the
## 	data set used for mining
newTrans <- sample(Income, 100)
m2 <- interestMeasure(rules, "support", transactions = newTrans, reuse = FALSE)
head(m2)

## calculate all available measures for the 5 frequent itemsets with highest support
its <- apriori(Income, parameter = list(target = "frequent itemsets"))
its <- head(its, 5, by = "support")
inspect(its)

interestMeasure(its, transactions = Income)
}
\references{
Hahsler, Michael (2015). A Probabilistic Comparison of Commonly Used
Interest Measures for Association Rules, 2015, URL:
\url{https://mhahsler.github.io/arules/docs/measures}.

Haldane, J.B.S. (1940). "The mean and variance of the moments of chi-squared
when used as a test of homogeneity, when expectations are small".
\emph{Biometrika,} 29, 133-134.

Anscombe, F.J. (1956). "On estimating binomial response relations".
\emph{Biometrika,} 43, 461-464.
}
\seealso{
\link{itemsets}, \link{rules}

Other interest measures: 
\code{\link{confint}()},
\code{\link{coverage}()},
\code{\link{is.redundant}()},
\code{\link{is.significant}()},
\code{\link{support}()}
}
\author{
Michael Hahsler
}
\concept{interest measures}
\keyword{models}
