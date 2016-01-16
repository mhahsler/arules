\name{read.PMML}
\alias{read.PMML}
\alias{write.PMML}
\title{Read and Write PMML}
\description{
  This function reads and writes PMML representations (version 4.1) of 
  associations (itemsets and rules).
}
\usage{
write.PMML(x, file)
read.PMML(file)
}
\arguments{
  \item{x}{a rules or itemsets object. }
  \item{file}{name of the PMML file (for read.PMML also a XML root node 
  can be supplied). }
}
\details{
Write delegates to package pmml.
}
\references{
  PMML home page: \url{http://www.dmg.org}
}
\seealso{
  \code{\link[pmml]{pmml}}.
}
\author{Michael Hahsler}
\examples{
data("Groceries")

rules <- apriori(Groceries, parameter=list(support=0.001))
rules <- head(rules, by="lift")
rules

### save rules as PMML
write.PMML(rules, file = "rules.xml")

### read rules back
rules2 <- read.PMML("rules.xml")
rules2

### compare rules
inspect(rules[1])
inspect(rules2[1])

### clean up
unlink("rules.xml")
}
\keyword{interface}
