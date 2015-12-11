\name{is.redundant}
\alias{is.redundant}
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
Zaki (2000) defines redundant rules as rules where a super rule (items added to the rule's LHS) have the same or higher confidence.

In this implementation other measures, e.g. lift, can be used as well. 
}
\value{
returns a logical vector indicating which rules are redundant.
}
%\seealso{
%}
\references{
Zaki, M. J. (2000) Generating non-redundant association rules. \emph{ Proceedings of the sixth ACM SIGKDD International Conference on Knowledge Discovery and Data Mining,} 34--42.
}
\examples{
data("Income")
rules <- apriori(Income, parameter = list(support = 0.5))
is.redundant(rules)

inspect(rules[!is.redundant(rules)])
}
\author{Michael Hahsler}
\keyword{manip}
