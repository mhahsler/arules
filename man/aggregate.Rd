\name{hierarchy}
\alias{hierarchy}
\alias{aggregate}
\alias{aggregate,itemMatrix-method}
\alias{aggregate,itemsets-method}
\alias{aggregate,rules-method}
\alias{addAggregate}
\alias{filterAggregate}
\title{Support for Item Hierarchies}
\description{
  Often an item hierarchy is available for datasets used for 
  association rule mining. For example in a supermarket dataset
  items like  "bread" and "beagle" might belong to the item
  group (category) "baked goods." 
  
  We provide support to use the item hierarchy to aggregate items 
  to different group levels, to
  produce multi-level transactions and to filter spurious    
  associations mined from multi-level transactions.
}
\usage{
\S4method{aggregate}{itemMatrix}(x, by)
\S4method{aggregate}{itemsets}(x, by)
\S4method{aggregate}{rules}(x, by)
addAggregate(x, by, postfix = "*")
filterAggregate(x)
}
\arguments{
  \item{x}{an transactions, itemsets or rules object.}
  \item{by}{name of a field (hierarchy level) available in 
    itemInfo or a vector of 
    character strings (factor) of the same length as items in \code{x}
    by which should be aggregated. Items receiving
    the same label in \code{by} will be aggregated into a single,
    higher-level item.}
  \item{postfix}{characters added to mark group-level items.}
}
\details{  
  Transactions can store item hierarchies as additional columns in the 
  itemInfo data.frame (\code{"labels"} is reserved for the item labels). 
  
  \bold{Aggregation:} To perform analysis at a group level of the item hierarchy,
  \code{aggregate()} produces a new
  object with items aggregated to a given group level. A group-level item
  is present if one or more of the items in the group are present in 
  the original object.
  If rules are aggregated, and the 
  aggregation would lead to the same aggregated group item in the lhs and 
  in the rhs, then that group item is removed from the lhs. 
  Rules or itemsets, which are not     
  unique after the aggregation, are also removed. Note also that the 
  quality measures are not applicable to the new rules and thus are removed. 
  If these measures are required, then aggregate the transactions before 
  mining rules.
  
  \bold{Multi-level analysis:} To analyze relationships between 
  individual items 
  and item groups at the same time, \code{addAggregate()} can be used to 
  create a new transactions object
  which contains both, the original items and group-level items (marked with
  a given postfix). In association rule mining, all items are handled 
  the same, which means that we will produce a large number of rules
  of the type 
  
            \deqn{item A => group of item A}

  with a confidence of 1. This will also happen if you mine itemsets. 
  \code{filterAggregate()} can be used to
  filter these spurious rules or itemsets.
}
\value{
   \code{aggregate()} returns an object of the same class as \code{x} encoded
   with a number of items equal to the number of unique values in
   \code{by}. Note that for associations (itemsets and rules) 
   the number of associations in the 
   returned set will most likely be reduced since
   several associations might map to the same aggregated association and
   aggregate returns a unique set. If several associations map to
   a single aggregated association then the quality measures of one 
   of the original associations is randomly chosen.
   
   \code{addAggregate()} returns a new transactions object with the
   original items and the group-items added. \code{filterAggregateRules()}
   returns a new rules object with the spurious rules remove.
}
\author{Michael Hahsler}
\examples{
data("Groceries")
Groceries
  
## Groceries contains a hierarchy stored in itemInfo
head(itemInfo(Groceries))

## aggregate by level2: items will become labels at level2
## Note that the number of items is therefore reduced to 55
Groceries_level2 <- aggregate(Groceries, by = "level2")
Groceries_level2
head(itemInfo(Groceries_level2)) ## labels are alphabetically sorted!


## compare orginal and aggregated transactions
inspect(head(Groceries, 2))
inspect(head(Groceries_level2, 2))

## create lables manually (organize items by the first letter)
mylevels <- toupper(substr(itemLabels(Groceries), 1, 1))
head(mylevels)

Groceries_alpha <- aggregate(Groceries, by = mylevels)
Groceries_alpha
inspect(head(Groceries_alpha, 2))

## aggregate rules 
## Note: you could also directly mine rules from aggregated transactions to
## get support, lift and support
rules <- apriori(Groceries, parameter=list(supp=0.005, conf=0.5))
rules
inspect(rules[1])

rules_level2 <- aggregate(rules, by = "level2")
inspect(rules_level2[1])

## mine multi-level rules:
## (1) add aggregate items. These items are followed by a *
Groceries_multilevel <- addAggregate(Groceries, "level2")
summary(Groceries_multilevel)
inspect(head(Groceries_multilevel))

rules <- apriori(Groceries_multilevel, 
  parameter = list(support = 0.01, conf = .9))
inspect(head(rules, by = "lift"))
## this contains many spurous rules of type 'item X => aggregare of item X'
## with a confidence of 1 and high lift.

## filter spurious rules resulting from the aggregation 
rules <- filterAggregate(rules)
inspect(head(rules, by = "lift"))
}
\keyword{manip}
