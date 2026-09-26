# Find Duplicated Elements

Provides the generic function `duplicated()` and the methods to find
duplicated elements in
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
and their subclasses.

## Usage

``` r
duplicated(x, incomparables = FALSE, ...)

# S4 method for class 'itemMatrix'
duplicated(x, incomparables = FALSE)

# S4 method for class 'rules'
duplicated(x, incomparables = FALSE)

# S4 method for class 'itemsets'
duplicated(x, incomparables = FALSE)
```

## Arguments

- x:

  an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  or
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md).

- incomparables:

  argument currently unused.

- ...:

  further arguments (currently unused).

## Value

A logical vector indicating duplicated elements.

## See also

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
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
[`rules-class`](http://michael.hahsler.net/arules/reference/rules-class.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

Other itemMatrix and transactions functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
[`image`](http://michael.hahsler.net/arules/reference/image.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md),
[`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md),
[`itemMatrix-class`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`random.transactions()`](http://michael.hahsler.net/arules/reference/random.transactions.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`supportingTransactions()`](http://michael.hahsler.net/arules/reference/supportingTransactions.md),
[`tidLists-class`](http://michael.hahsler.net/arules/reference/tidLists-class.md),
[`transactions-class`](http://michael.hahsler.net/arules/reference/transactions-class.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
data("Adult")

r1 <- apriori(Adult[1:1000], parameter = list(support = 0.5))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.5      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 500 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[100 item(s), 1000 transaction(s)] done [0.00s].
#> sorting and recoding items ... [9 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 done [0.00s].
#> writing ... [129 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
r2 <- apriori(Adult[1001:2000], parameter = list(support = 0.5))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.5      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 500 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[101 item(s), 1000 transaction(s)] done [0.00s].
#> sorting and recoding items ... [9 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 done [0.00s].
#> writing ... [114 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].

## Note this creates a collection of rules from two sets of rules
r_comb <- c(r1, r2)
duplicated(r_comb)
#>   [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  [97] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [109] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [121] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
#> [133]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [145]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [157]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [169]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [181]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [193]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [205]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [217]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [229]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [241]  TRUE  TRUE  TRUE
```
