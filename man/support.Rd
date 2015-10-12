\name{support}
\alias{support}
\alias{support,itemMatrix-method}
\alias{support,associations-method}
\title{Support Counting for Itemsets}
\description{
   Provides the generic function and the needed S4 method to count support for
   given itemsets (and other types of associations) in a given transaction
   database. 
}
\usage{
support(x, transactions, ...)
\S4method{support}{itemMatrix}(x, transactions, 
    type= c("relative", "absolute"), weighted = FALSE, control = NULL)
\S4method{support}{associations}(x, transactions, 
    type= c("relative", "absolute"), weighted = FALSE, control = NULL)

}
\arguments{
  \item{x}{ the set of itemsets for which support should be counted. }
  \item{\dots}{ further arguments are passed on. }
  \item{transactions}{ the transaction data set used for mining. }
  \item{type}{ a character string specifying
    if \code{"relative"}  support or  \code{"absolute"}
    support (counts) are returned for the itemsets in \code{x}. 
    (default:  \code{"relative"})}
  \item{weighted}{ should support be weighted by transactions weights stored
   as column \code{"weight"} in transactionInfo?} 
  \item{control}{a named list with elements 
    \code{method} indicating the method (\code{"tidlists"} or \code{"ptree"}),
    and the logical arguments
    \code{reduce} and
    \code{verbose} to indicate if unused items are removed and if 
    the output should be verbose.}
}
\details{
   Normally, itemset support is counted during mining the database
   with a set minimum support. However, if only the support information 
   for a single or a few itemsets is needed, one might not want to 
   mine the database for all frequent itemsets.
   
   If in control \code{method = "ptree"} is used, 
   the counters for the itemsets are 
   organized in a prefix tree. The transactions are sequentially processed
   and the corresponding counters in the prefix tree are incremented 
   (see Hahsler et al, 2008). This method is used by default since it is 
   typically significantly faster than tid list intersection.
   
   If in control \code{method = "tidlists"} is used,
   support is counted using transaction ID list intersection
   which is used by several fast mining algorithms
   (e.g., by Eclat). However, Support is determined for each itemset
   individually which is slow for a large number of long itemsets
   in dense data. 
   
   If in control \code{reduce = TRUE} is used, unused items are removed from
   the data before creating rules. This might be slower for large transaction
   data sets.
   }
\value{
   A numeric vector of the same length as \code{x} containing
   the support values for the sets in \code{x}.
}

\seealso{ 
\code{\link{itemMatrix-class}}, 
\code{\link{associations-class}}, 
\code{\link{transactions-class}}
}
\references{
Michael Hahsler, Christian Buchta, and Kurt Hornik. Selective association 
rule generation. \emph{Computational Statistics}, 23(2):303-315, April 2008.
}
\author{Michael Hahsler and Christian Buchta}
\examples{
data("Income")

## find and some frequent itemsets
itemsets <- eclat(Income)[1:5]

## inspect the support returned by eclat
inspect(itemsets)

## count support in the database
support(items(itemsets), Income)
}
\keyword{models}
