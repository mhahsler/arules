\name{hierarchy}
\alias{aggregate}
\alias{aggregate,itemMatrix-method}
\alias{aggregate,itemsets-method}
\alias{aggregate,rules-method}
\title{Aggregate Items Into Hierarchical Item Groups}
\description{
  Provides the generic functions and the S4 methods for 
  aggregating items in rules and itemsets into hierarchical groups.

  Often an item hierarchy is available for datasets used for 
  association rule mining. For example in a supermarket dataset
  items like  "bread" and "beagle" might belong to the item
  group (category) "baked goods". The aggregate methods
  replaces items in transactions, itemsets or rules with
  item groups as specified by the user.
}
\usage{
\S4method{aggregate}{itemMatrix}(x, itemLabels)
\S4method{aggregate}{itemsets}(x, itemLabels)
\S4method{aggregate}{rules}(x, itemLabels)
}
\arguments{
    \item{x}{ 
	an transactions, itemsets or rules object.}
    \item{itemLabels}{name of a field available in itemInfo or a vector of character strings (factor)
    of the same length as items in \code{x}. Items receiving
    the same item label will be aggregated.}
}
\details{  
  Transactions can store item hierarchies as additional columns in the 
  itemInfo data.frame (\code{"labels"} is reserved for the item labels). These
  item hierarchies can be used for aggregation.
  
  If rules are aggregated and the aggregation would
  lead to the same item in the lhs and in the rhs then the item
  is removed from the lhs. Rules or itemsets which are not     
  unique after the aggregation are also removed.
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
Groceries
  
## Groceries contains a hierarchy stored in itemInfo
head(itemInfo(Groceries))

## aggregate by level1
Groc_level1 <- aggregate(Groceries, "level1")
Groc_level1
inspect(head(Groc_level1))

## create lables manually (organize items by the first letter)
mylevels <- toupper(substr(itemLabels(Groceries), 1, 1))
head(mylevels)

Groc_alpha <- aggregate(Groceries, mylevels)
Groc_alpha
inspect(head(Groc_alpha))


## Aggregate rules
rules <- apriori(Groceries, parameter=list(supp=0.005, conf=0.5))
rules
inspect(rules[1])

rules_level1 <- aggregate(rules, "level1")
inspect(rules_level1[1])
}
\keyword{manip}
