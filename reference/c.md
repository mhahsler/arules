# Combining Association and Transaction Objects

Provides the methods to combine several
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
or
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
objects into a single object.

## Usage

``` r
# S4 method for class 'itemMatrix'
c(x, ..., recursive = FALSE)

# S4 method for class 'transactions'
c(x, ..., recursive = FALSE)

# S4 method for class 'tidLists'
c(x, ..., recursive = FALSE)

# S4 method for class 'rules'
c(x, ..., recursive = FALSE)

# S4 method for class 'itemsets'
c(x, ..., recursive = FALSE)
```

## Arguments

- x:

  first object.

- ...:

  further objects of the same class as `x` to be combined.

- recursive:

  a logical. If `recursive = TRUE`, the function recursively descends
  through lists combining all their elements into a vector.

## Value

An object of the same class as `x`.

## Details

Combining arules objects is done by combining the rows of
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
objects representing the associations or transactions.

Note that `c()` can result in duplicates. Use
[`union()`](http://michael.hahsler.net/arules/reference/sets.md) rather
than `c()` to combine several mined
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
or [rules](http://michael.hahsler.net/arules/reference/rules-class.md)
into a single set without duplicates.

## See also

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
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
[`rules-class`](http://michael.hahsler.net/arules/reference/rules-class.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

Other itemMatrix and transactions functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
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

## combine transactions
a1 <- Adult[1:10]
a2 <- Adult[101:110]

aComb <- c(a1, a2)
summary(aComb)
#> transactions as itemMatrix in sparse format with
#>  20 rows (elements/itemsets/transactions) and
#>  115 columns (items) and a density of 0.1121739 
#> 
#> most frequent items:
#>            capital-loss=None native-country=United-States 
#>                           20                           18 
#>                   race=White            capital-gain=None 
#>                           17                           14 
#>     hours-per-week=Full-time                      (Other) 
#>                           14                          175 
#> 
#> element (itemset/transaction) length distribution:
#> sizes
#> 11 13 
#>  1 19 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    11.0    13.0    13.0    12.9    13.0    13.0 
#> 
#> includes extended item information - examples:
#>            labels variables      levels
#> 1       age=Young       age       Young
#> 2 age=Middle-aged       age Middle-aged
#> 3      age=Senior       age      Senior
#> 
#> includes extended transaction information - examples:
#>   transactionID
#> 1             1
#> 2             2
#> 3             3

## combine rules (can contain the same rule multiple times)
r1 <- apriori(Adult[1:1000])
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.1      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 100 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[100 item(s), 1000 transaction(s)] done [0.00s].
#> sorting and recoding items ... [31 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 6 7 8 done [0.01s].
#> writing ... [8500 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
r2 <- apriori(Adult[1001:2000])
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.1      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 100 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[101 item(s), 1000 transaction(s)] done [0.00s].
#> sorting and recoding items ... [30 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 6 7 8 9 done [0.01s].
#> writing ... [8575 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
rComb <- c(r1, r2)
rComb
#> set of 17075 rules 

## union of rules (a set with only unique rules: same as unique(rComb))
rUnion <- union(r1, r2)
rUnion
#> set of 9928 rules 
```
