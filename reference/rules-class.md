# Class rules — A Set of Rules

Defines the `rules` class to represent a set of association rules and
methods to work with `rules`.

## Usage

``` r
rules(rhs, lhs, itemLabels = NULL, quality = data.frame())

# S4 method for class 'rules'
summary(object, ...)

# S4 method for class 'rules'
length(x)

# S4 method for class 'rules'
nitems(x)

# S4 method for class 'rules'
labels(object, ruleSep = " => ", ...)

# S4 method for class 'rules'
itemLabels(object)

# S4 method for class 'rules'
itemLabels(object) <- value

# S4 method for class 'rules'
itemInfo(object)

lhs(x)

# S4 method for class 'rules'
lhs(x)

lhs(x) <- value

# S4 method for class 'rules'
lhs(x) <- value

rhs(x)

rhs(x) <- value

# S4 method for class 'rules'
rhs(x) <- value

# S4 method for class 'rules'
rhs(x)

# S4 method for class 'rules'
items(x)

generatingItemsets(x)

# S4 method for class 'rules'
generatingItemsets(x)
```

## Arguments

- rhs, lhs:

  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  objects or objects that can be converted using
  [`encode()`](http://michael.hahsler.net/arules/reference/itemCoding.md).

- itemLabels:

  a vector of all possible item labels (character) or a transactions
  object to copy the item coding used for
  [`encode()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  (see
  [itemCoding](http://michael.hahsler.net/arules/reference/itemCoding.md)
  for details).

- quality:

  a data.frame with quality information (one row per rule).

- object, x:

  the object

- ...:

  further arguments

- ruleSep:

  rule separation symbol

- value:

  replacement value

## Details

Mined rule sets typically contain several interest measures accessible
with the
[`quality()`](http://michael.hahsler.net/arules/reference/associations-class.md)
method. Additional measures can be calculated via
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md).

To create rules manually, the itemMatrix for the LHS and the RHS of the
rules need to be compatible. See
[itemCoding](http://michael.hahsler.net/arules/reference/itemCoding.md)
for details.

## Functions

- `summary(rules)`: create a summary

- `length(rules)`: returns the number of rules.

- `nitems(rules)`: returns the number of items used in the current
  encoding.

- `labels(rules)`: labels for the rules.

- `itemLabels(rules)`: returns item labels for the current encoding.

- `itemLabels(rules) <- value`: change the item labels in the current
  encoding.

- `itemInfo(rules)`: returns the item info data.frame.

- `lhs(rules)`: returns the LHS of the rules as an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

- `lhs(rules) <- value`: replaces the LHS of the rules with an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

- `rhs(rules) <- value`: replaces the RHS of the rules with an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

- `rhs(rules)`: returns the RHS of the rules as an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

- `items(rules)`: returns all items in a rule (LHS and RHS) an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

- `generatingItemsets(rules)`: returns a collection of the itemsets
  which generated the rules, one itemset for each rule. Note that the
  collection can be a multiset and contain duplicated elements. Use
  [`unique()`](http://michael.hahsler.net/arules/reference/unique.md) to
  remove duplicates and obtain a proper set. This method produces the
  same as the result as calling
  [`items()`](http://michael.hahsler.net/arules/reference/associations-class.md),
  but wrapped into an
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  object with support information.

## Slots

- `lhs,rhs`:

  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  representing the left-hand-side and right-hand-side of the rules.

- `quality`:

  the quality data.frame

- `info`:

  a list with mining information.

## Objects from the Class

Objects are the result of calling the function
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md).
Objects can also be created by calls of the form `new("rules", ...)` or
by using the constructor function `rules()`.

## Coercions

- `as("rules", "data.frame")`

## See also

Superclass:
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`itemsets-class`](http://michael.hahsler.net/arules/reference/itemsets-class.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
data("Adult")

## Mine rules
rules <- apriori(Adult, parameter = list(support = 0.3))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.3      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 14652 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [14 item(s)] done [0.00s].
#> creating transaction tree ... done [0.01s].
#> checking subsets of size 1 2 3 4 5 6 done [0.00s].
#> writing ... [508 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
rules
#> set of 508 rules 

## Select a subset of rules using partial matching on the items
## in the right-hand-side and a quality measure
rules.sub <- subset(rules, subset = rhs %pin% "sex" & lift > 1.3)

## Display the top 3 support rules
inspect(head(rules.sub, n = 3, by = "support"))
#>     lhs                                     rhs          support confidence  coverage     lift count
#> [1] {marital-status=Married-civ-spouse}  => {sex=Male} 0.4074157  0.8891818 0.4581917 1.330151 19899
#> [2] {relationship=Husband}               => {sex=Male} 0.4036485  0.9999493 0.4036690 1.495851 19715
#> [3] {marital-status=Married-civ-spouse,                                                             
#>      relationship=Husband}               => {sex=Male} 0.4034028  0.9999492 0.4034233 1.495851 19703

## Display the first 3 rules
inspect(rules.sub[1:3])
#>     lhs                                     rhs          support confidence  coverage     lift count
#> [1] {relationship=Husband}               => {sex=Male} 0.4036485  0.9999493 0.4036690 1.495851 19715
#> [2] {marital-status=Married-civ-spouse}  => {sex=Male} 0.4074157  0.8891818 0.4581917 1.330151 19899
#> [3] {marital-status=Married-civ-spouse,                                                             
#>      relationship=Husband}               => {sex=Male} 0.4034028  0.9999492 0.4034233 1.495851 19703

## Get labels for the first 3 rules
labels(rules.sub[1:3])
#> [1] "{relationship=Husband} => {sex=Male}"                                  
#> [2] "{marital-status=Married-civ-spouse} => {sex=Male}"                     
#> [3] "{marital-status=Married-civ-spouse,relationship=Husband} => {sex=Male}"
labels(rules.sub[1:3],
  itemSep = " + ", setStart = "", setEnd = "",
  ruleSep = " ---> "
)
#> [1] "relationship=Husband ---> sex=Male"                                    
#> [2] "marital-status=Married-civ-spouse ---> sex=Male"                       
#> [3] "marital-status=Married-civ-spouse + relationship=Husband ---> sex=Male"

## Manually create rules using the item coding in Adult and calculate some interest measures
twoRules <- rules(
  lhs = list(
    c("age=Young", "relationship=Unmarried"),
    c("age=Old")
  ),
  rhs = list(
    c("income=small"),
    c("income=large")
  ),
  itemLabels = Adult
)

quality(twoRules) <- interestMeasure(twoRules,
  measure = c("support", "confidence", "lift"), transactions = Adult
)

inspect(twoRules)
#>     lhs                                    rhs            support    
#> [1] {age=Young, relationship=Unmarried} => {income=small} 0.006940748
#> [2] {age=Old}                           => {income=large} 0.004770484
#>     confidence lift     
#> [1] 0.6608187  1.3056516
#> [2] 0.1292291  0.8049746
```
