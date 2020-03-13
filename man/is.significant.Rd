\name{is.significant}
\alias{is.significant}
\alias{is.significant,rules-method}
%
%
\title{Find Significant Rules}
\description{
  Provides the generic functions and the S4 method \code{is.significant} 
  to find significant associations and an implementation for rules.
}
\usage{
is.significant(x, transactions, method = "fisher", 
  alpha = 0.01, adjust = "bonferroni") 
}
\arguments{
  \item{x}{ a set of rules.}
  \item{transactions}{set of transactions used to mine the rules. }
  \item{method}{ test to use. Options are
  \code{"fisher", "chisq"}. Note that the contingency table is likely to have cells with low expected values and that thus Fisher's Exact Test might be more appropriate than the chi-squared test.}
  \item{alpha}{required significance level.}
  \item{adjust}{method to adjust for multiple comparisons. Options are
  \code{"none", "bonferroni", "holm", "fdr"}, etc. (see \code{\link[stats]{p.adjust}})}
}
\details{
  The implementation for association rules 
  uses Fisher's exact test with correction for multiple comparisons
  to test the null hypothesis that the LHS and the RHS of the rule are independent.
  Significant rules have a p-value less then the specified significance level alpha 
  (the null hypothesis of independence is rejected.).
}
\value{
returns a logical vector indicating which rules are significant.
}
\seealso{
  \code{\link{interestMeasure}},
  \code{\link[stats]{p.adjust}}
}
\references{
Hahsler, Michael and Kurt Hornik (2007). New probabilistic interest measures for association rules. \emph{Intelligent Data Analysis}, 11(5):437--455.
}
\examples{
data("Income")
rules <- apriori(Income, parameter = list(support = 0.5))
is.significant(rules, Income)

inspect(rules[is.significant(rules, Income)])
}
\author{Michael Hahsler}
\keyword{manip}
