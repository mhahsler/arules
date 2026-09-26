# Mining and pruning association rules

Association rule mining can produce more rules than are practical to
inspect. An effective workflow constrains the search, filters and ranks
the result, and then removes rules that add no information.

``` r

trans <- transactions(list(
  T1 = c("bread", "butter", "milk"),
  T2 = c("bread", "butter"),
  T3 = c("bread", "milk"),
  T4 = c("bread", "butter", "jam"),
  T5 = c("bread", "butter", "milk"),
  T6 = c("butter", "jam"),
  T7 = c("bread", "milk", "cereal"),
  T8 = c("bread", "butter", "jam")
))
```

## Constrain the search

Support, confidence, and rule length constrain the rule set while
Apriori is searching. The `appearance` argument can also restrict items
to the left- or right-hand side. Here, Apriori generates only rules that
predict `butter` or `milk`.

``` r

rules <- apriori(
  trans,
  parameter = list(
    support = 0.25, confidence = 0.6,
    maxlen = 3
  ),
  appearance = list(
    rhs = c("butter", "milk"),
    default = "lhs"
  )
)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.6    0.1    1 none FALSE            TRUE       5    0.25      1
#>  maxlen target  ext
#>       3  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 2 
#> 
#> set item appearances ...[2 item(s)] done [0.00s].
#> set transactions ...[5 item(s), 8 transaction(s)] done [0.00s].
#> sorting and recoding items ... [4 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 done [0.00s].
#> writing ... [4 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
inspect(rules)
#>     lhs             rhs      support confidence coverage lift     count
#> [1] {}           => {butter} 0.750   0.7500000  1.000    1.000000 6    
#> [2] {jam}        => {butter} 0.375   1.0000000  0.375    1.333333 3    
#> [3] {bread}      => {butter} 0.625   0.7142857  0.875    0.952381 5    
#> [4] {bread, jam} => {butter} 0.250   1.0000000  0.250    1.333333 2
```

These constraints produce only 4 rules. Constraining the search also
reduces its memory and computation requirements.

## Rank and filter

Filter by criteria appropriate for the task, then rank the remaining
rules. Keeping these criteria in the code makes the selection
reproducible.

``` r

selected <- subset(rules, lift > 1 & confidence >= 0.7)
ranked <- sort(selected, by = "lift", decreasing = TRUE)
inspect(ranked)
#>     lhs             rhs      support confidence coverage lift     count
#> [1] {jam}        => {butter} 0.375   1          0.375    1.333333 3    
#> [2] {bread, jam} => {butter} 0.250   1          0.250    1.333333 2
```

Many interest measures are available in addition to support, confidence,
and lift. The vignette [Interest
measures](http://michael.hahsler.net/arules/articles/interest-measures.md)
([`vignette("interest-measures", package = "arules")`](http://michael.hahsler.net/arules/articles/interest-measures.md))
introduces the use of additional interest measures.

## Remove redundant rules

A rule is redundant if a more general rule with the same consequent
performs at least as well according to the selected measure. Removing
redundant rules produces a more concise result.

``` r

non_redundant <- rules[!is.redundant(rules)]
inspect(sort(non_redundant, by = "lift"))
#>     lhs      rhs      support confidence coverage lift     count
#> [1] {jam} => {butter} 0.375   1.00       0.375    1.333333 3    
#> [2] {}    => {butter} 0.750   0.75       1.000    1.000000 6
```

The complementary subset contains the redundant rules that were removed.

``` r

inspect(rules[is.redundant(rules)])
#>     lhs             rhs      support confidence coverage lift     count
#> [1] {bread}      => {butter} 0.625   0.7142857  0.875    0.952381 5    
#> [2] {bread, jam} => {butter} 0.250   1.0000000  0.250    1.333333 2
```
