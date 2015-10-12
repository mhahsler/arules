\name{apriori}
\alias{apriori}
\title{Mining Associations with Apriori}
\description{
  Mine frequent itemsets, association rules or association hyperedges
  using the Apriori algorithm.  The Apriori algorithm employs level-wise
  search for frequent itemsets.  The implementation of Apriori used
  includes some improvements (e.g., a prefix tree and item sorting).
}
\usage{
apriori(data, parameter = NULL, appearance = NULL, control = NULL)
}
\arguments{
  \item{data}{object of class
    \code{\linkS4class{transactions}} or any data structure
    which can be coerced into
    \code{\linkS4class{transactions}} (e.g., a binary
    matrix or data.frame).}
  \item{parameter}{object of class
    \code{\linkS4class{APparameter}} or named list.
    The default behavior is to mine rules with support 0.1, confidence
    0.8, and maxlen 10.}
  \item{appearance}{object of class
    \code{\linkS4class{APappearance}} or named list.
    With this argument item appearance can be restricted.
    By default all items can appear unrestricted.}
  \item{control}{object of class
    \code{\linkS4class{APcontrol}} or named list.
    Controls the performance of the mining algorithm (item sorting, etc.)}
}
\details{
  Calls the C implementation of the Apriori algorithm by Christian
  Borgelt for mining frequent itemsets, rules or hyperedges.

  Note: Apriori only creates rules with one item in the RHS (Consequent)!

  Note: The default value in \code{\linkS4class{APparameter}} for \code{minlen} is 1. This
  means that rules with only one item (i.e., an empty antecedent/LHS) like
  
  \deqn{\{\} => \{beer\}}{{} => {beer}}
  
  will be created. 
  These rules mean that no matter what other items are involved the 
  item in the RHS will appear with the probability given by the rule's
  confidence (which equals the support).
  If you want to avoid these rules then use 
  the argument \code{parameter=list(minlen=2)}.
}
\value{
  Returns an object of class \code{\linkS4class{rules}} or
  \code{\linkS4class{itemsets}}.
}
\references{
  R. Agrawal, T. Imielinski, and A. Swami (1993) Mining association rules
  between sets of items in large databases. In \emph{Proceedings of the
    ACM SIGMOD International Conference on Management of Data}, pages
  207--216,
  Washington D.C.
  
  Christian Borgelt and Rudolf Kruse (2002) Induction of Association Rules:
  Apriori Implementation. \emph{15th Conference on Computational
    Statistics} (COMPSTAT 2002, Berlin, Germany) Physica Verlag,
  Heidelberg, Germany.

  Christian Borgelt (2003) Efficient Implementations of Apriori and
  Eclat.  \emph{Workshop of Frequent Item Set Mining Implementations}
  (FIMI 2003, Melbourne, FL, USA).

  APRIORI Implementation: \url{www.borgelt.net/apriori.html}
}
\author{Michael Hahsler and Bettina Gruen}
\examples{
data("Adult")
## Mine association rules.
rules <- apriori(Adult, 
	parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)
}
\seealso{
  \code{\link{APparameter-class}},
  \code{\link{APcontrol-class}},
  \code{\link{APappearance-class}},
  \code{\link{transactions-class}},
  \code{\link{itemsets-class}},
  \code{\link{rules-class}}
}
\keyword{models}
