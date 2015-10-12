\name{aggregate}
\alias{aggregate}
\alias{aggregate,itemMatrix-method}
\alias{aggregate,itemsets-method}
\alias{aggregate,rules-method}
\title{Aggregate Items Into Item Groups}
\description{
  Provides the generic functions and the S4 methods for 
  aggregating items in rules and itemsets.
  
  Often an item hierarchy is available for datasets used for 
  association rule mining. For example in a supermarket dataset
  items like  "bread" and "beagle" might belong to the item
  group (category) "baked goods". The aggregate methods
  replaces items in transactions, itemsets or rules with
  item groups as specified by the user.
  
  If rules are aggregated and the aggregation would
  lead to the same item in the lhs and in the rhs then the item
  is removed from the lhs. Rules or itemsets which are not     
  unique after the aggregation are also removed.
}
\usage{
\S4method{aggregate}{itemMatrix}(x, itemLabels)
\S4method{aggregate}{itemsets}(x, itemLabels)
\S4method{aggregate}{rules}(x, itemLabels)
}
\arguments{
    \item{x}{ 
	an transactions, itemsets or rules object.}
    \item{itemLabels}{a vector of character strings (factor)
    of the same length as items in \code{x}. Items receiving
    the same item label will be aggregated.}
}
\value{
   This method returns an object of the same class as \code{x} encoded
   with a number of items equal to the number of unique values in
   \code{itemLabels}. Note that for associations (itemsets and rules) 
   the number of associations in the 
   returned set will most likely be reduced since
   several associations might map to the same aggregated association and
   aggregate returns a unique set. If several associations map to
   a single aggregated association then the quality measures of one 
   of the original associations is randomly chosen.
}
\author{Michael Hahsler}
\examples{
data("Groceries")

## Groceries contains a hierarchy stored in itemInfo
head(itemInfo(Groceries))
level1 <- itemInfo(Groceries)[["level1"]]
levels(level1)


## Aggregate transactions
aggregate(Groceries, level1)

## Aggregate rules
rules <- apriori(Groceries, parameter=list(supp=0.005, conf=0.5))
rules
aggregate(rules, level1)
}
\keyword{manip}
