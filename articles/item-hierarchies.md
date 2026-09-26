# Item hierarchies

Often, we deal with many items. For example in a supermarket there may
be 10s of different types of milk and 100s of different dairy products.
An item hierarchy maps detailed items to broader groups. For example,
milk and yogurt are grouped in the dairy category. This information will
help us analyze when rules are true for a whole category.

Item hierarchy level are stored as a column in
[`itemInfo()`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
in the transactions.

``` r

trans <- transactions(list(
  T1 = c("apple", "banana"),
  T2 = c("apple", "yogurt"),
  T3 = c("banana", "milk"),
  T4 = c("apple", "banana", "milk"),
  T5 = c("milk", "yogurt"),
  T6 = c("apple", "banana", "yogurt")
))
itemInfo(trans)
#>   labels
#> 1  apple
#> 2 banana
#> 3   milk
#> 4 yogurt
```

Initially, the transactions only contain the item labels. We can add a
category as an level in the hierarchy.

``` r

category_list <- c(
  apple = "fruit", banana = "fruit",
  milk = "dairy", yogurt = "dairy"
)
itemInfo(trans)$category <- category_list[itemLabels(trans)]
itemInfo(trans)
#>   labels category
#> 1  apple    fruit
#> 2 banana    fruit
#> 3   milk    dairy
#> 4 yogurt    dairy
```

## Aggregating transactions

[`aggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
replaces item labels with their group. Multiple items from the same
group in one basket become a single group item.

``` r

by_category <- aggregate(trans, by = "category")
inspect(trans)
#>     items                   transactionID
#> [1] {apple, banana}         T1           
#> [2] {apple, yogurt}         T2           
#> [3] {banana, milk}          T3           
#> [4] {apple, banana, milk}   T4           
#> [5] {milk, yogurt}          T5           
#> [6] {apple, banana, yogurt} T6
inspect(by_category)
#>     items          transactionID
#> [1] {fruit}        T1           
#> [2] {dairy, fruit} T2           
#> [3] {dairy, fruit} T3           
#> [4] {dairy, fruit} T4           
#> [5] {dairy}        T5           
#> [6] {dairy, fruit} T6
itemFrequency(by_category)
#>     dairy     fruit 
#> 0.8333333 0.8333333
```

Mine the aggregated transactions when the analysis is intended to
operate only at the group level. This calculates valid quality measures
for that level.

``` r

category_rules <- apriori(
  by_category,
  parameter = list(support = 0.3, confidence = 0.5, minlen = 2),
  control = list(verbose = FALSE)
)
inspect(category_rules)
#>     lhs        rhs     support   confidence coverage  lift count
#> [1] {fruit} => {dairy} 0.6666667 0.8        0.8333333 0.96 4    
#> [2] {dairy} => {fruit} 0.6666667 0.8        0.8333333 0.96 4
```

## Mine across hierarchy levels

[`addAggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
retains items and adds group items. By default, group items are marked
with an asterisk. This creates rules that cross levels.

``` r

multilevel <- addAggregate(trans, by = "category")
inspect(multilevel)
#>     items                                   transactionID
#> [1] {apple, banana, fruit*}                 T1           
#> [2] {apple, yogurt, dairy*, fruit*}         T2           
#> [3] {banana, milk, dairy*, fruit*}          T3           
#> [4] {apple, banana, milk, dairy*, fruit*}   T4           
#> [5] {milk, yogurt, dairy*}                  T5           
#> [6] {apple, banana, yogurt, dairy*, fruit*} T6

multilevel_rules <- apriori(
  multilevel,
  parameter = list(support = 0.1, confidence = 0.6, minlen = 2),
  control = list(verbose = FALSE)
)
multilevel_rules
#> set of 83 rules
```

Adding category items creates trivial rules such as
`{apple} => {fruit*}` because the hierarchy guarantees them.
[`filterAggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
removes associations that contain both a detailed item and its own
aggregate. This reduces the size of the rule set significantly and makes
it easier to interpret.

``` r

multilevel_rules <- filterAggregate(multilevel_rules)
multilevel_rules
#> set of 17 rules
  
inspect(sort(multilevel_rules, by = "lift"))
#>      lhs                 rhs      support   confidence coverage  lift  count
#> [1]  {banana, yogurt} => {apple}  0.1666667 1.0000000  0.1666667 1.500 1    
#> [2]  {apple, milk}    => {banana} 0.1666667 1.0000000  0.1666667 1.500 1    
#> [3]  {banana}         => {apple}  0.5000000 0.7500000  0.6666667 1.125 3    
#> [4]  {apple}          => {banana} 0.5000000 0.7500000  0.6666667 1.125 3    
#> [5]  {yogurt}         => {apple}  0.3333333 0.6666667  0.5000000 1.000 2    
#> [6]  {milk}           => {banana} 0.3333333 0.6666667  0.5000000 1.000 2    
#> [7]  {banana, dairy*} => {apple}  0.3333333 0.6666667  0.5000000 1.000 2    
#> [8]  {apple, dairy*}  => {banana} 0.3333333 0.6666667  0.5000000 1.000 2    
#> [9]  {fruit*}         => {dairy*} 0.6666667 0.8000000  0.8333333 0.960 4    
#> [10] {dairy*}         => {fruit*} 0.6666667 0.8000000  0.8333333 0.960 4    
#> [11] {banana}         => {dairy*} 0.5000000 0.7500000  0.6666667 0.900 3    
#> [12] {apple}          => {dairy*} 0.5000000 0.7500000  0.6666667 0.900 3    
#> [13] {dairy*}         => {banana} 0.5000000 0.6000000  0.8333333 0.900 3    
#> [14] {dairy*}         => {apple}  0.5000000 0.6000000  0.8333333 0.900 3    
#> [15] {yogurt}         => {fruit*} 0.3333333 0.6666667  0.5000000 0.800 2    
#> [16] {milk}           => {fruit*} 0.3333333 0.6666667  0.5000000 0.800 2    
#> [17] {apple, banana}  => {dairy*} 0.3333333 0.6666667  0.5000000 0.800 2
```

## Other vignettes

- [Getting started with
  arules](http://michael.hahsler.net/arules/articles/getting-started.md)
- [Preparing transaction
  data](http://michael.hahsler.net/arules/articles/preparing-transaction-data.md)
- [Mining and pruning association
  rules](http://michael.hahsler.net/arules/articles/mining-and-pruning-rules.md)
- [Interest
  measures](http://michael.hahsler.net/arules/articles/interest-measures.md)
