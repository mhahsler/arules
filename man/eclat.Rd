\name{eclat}
\alias{eclat}
\title{Mining Associations with Eclat}
\description{
  Mine frequent itemsets with the Eclat algorithm.
  This algorithm uses simple intersection operations for equivalence
  class clustering along with bottom-up lattice traversal.
}
\usage{
eclat(data, parameter = NULL, control = NULL)
}
\arguments{
 \item{data}{object of class
   \code{\linkS4class{transactions}} or any data structure
   which can be coerced into
   \code{\linkS4class{transactions}} (e.g., binary
   \code{matrix}, \code{data.frame}).}
 \item{parameter}{object of class
   \code{\linkS4class{ECparameter}} or named list (default
   values are: support 0.1 and maxlen 5)}
  \item{control}{object of class
    \code{\linkS4class{ECcontrol}} or named list for
    algorithmic controls.}
}
\details{
  Calls the C implementation of the Eclat algorithm by Christian 
  Borgelt for mining frequent itemsets. 

  Eclat can also return the transaction IDs for each found itemset using 
  \code{tidLists=TRUE} as a parameter and the result can be retrieved 
  as a \code{\linkS4class{tidLists}} object with method
  \code{tidLists()} for class \code{\linkS4class{itemsets}}. 
  Note that storing transaction ID lists is very memory intensive, 
  creating transaction ID lists only works for minimum
  support values which create a relatively small number of itemsets. 
  See also \code{\link{supportingTransactions}}.

  \code{\link{ruleInduction}} can be used to generate rules from the found itemsets.

  A weighted version of ECLAT is available as function \code{\link{weclat}}. 
  This version can be used to perform weighted association rule mining (WARM).
}
\value{
  Returns an object of class \code{\linkS4class{itemsets}}.
}
\references{
  Mohammed J. Zaki, Srinivasan Parthasarathy, Mitsunori Ogihara, and Wei
  Li. (1997)
  \emph{New algorithms for fast discovery of association rules}.
  KDD'97: Proceedings of the Third International Conference on Knowledge Discovery and Data Mining,
  August 1997, Pages 283-286. 
  
  Christian Borgelt (2003) Efficient Implementations of Apriori and
  Eclat. \emph{Workshop of Frequent Item Set Mining Implementations}
  (FIMI 2003, Melbourne, FL, USA).

  ECLAT Implementation: \url{https://borgelt.net/eclat.html}
}
\seealso{
  \code{\link{ECparameter-class}},
  \code{\link{ECcontrol-class}},
  \code{\link{transactions-class}},
  \code{\link{itemsets-class}},
  \code{\link{weclat}},
  \code{\link{apriori}},
  \code{\link{ruleInduction}},
  \code{\link{supportingTransactions}}
}
\author{Michael Hahsler and Bettina Gruen}
\examples{
data("Adult")
## Mine itemsets with minimum support of 0.1 and 5 or less items
itemsets <- eclat(Adult,
		parameter = list(supp = 0.1, maxlen = 5))
itemsets

## Create rules from the itemsets
rules <- ruleInduction(itemsets, Adult, confidence = .9)
rules
}
\keyword{models}
