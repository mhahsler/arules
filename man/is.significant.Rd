\name{is.significant}
\alias{is.significant}
\alias{is.significant,rules-method}
%
%
\title{Find Significant Rules}
\description{
  Provides the generic functions and the S4 method \code{is.significant} 
  to find rules where the LHS and the RHS depend on each other. This uses
  Fisher's exact test and corrects for multiple comparisons.
}
\usage{
is.significant(x, transactions, alpha = 0.01, adjust = "bonferroni") 
}
\arguments{
  \item{x}{ a set of rules.}
  \item{transactions}{set of transactions used to mine the rules. }
  \item{alpha}{required significance level.}
  \item{adjust}{method to adjust for multiple comparisons. Options are
  \code{"none", "bonferroni", "holm", "fdr"}, etc. (see \code{\link[stats]{p.adjust}})}
}
%\details{
%}
\value{
returns a logical vector indicating which rules are significant.
}
\seealso{
  \code{\link{interestMeasure}},
  \code{\link[stats]{p.adjust}}
}
\examples{
data("Income")
rules <- apriori(Income, parameter = list(support = 0.5))
is.significant(rules, Income)

inspect(rules[is.significant(rules, Income)])
}
\author{Michael Hahsler}
\keyword{manip}
