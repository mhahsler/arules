# Subsetting Itemsets, Rules and Transactions

Provides the generic function `subset()` and methods to subset
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
or
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
([itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md))
which meet certain conditions (e.g., contains certain items or satisfies
a minimum lift).

## Usage

``` r
subset(x, ...)

# S4 method for class 'itemMatrix'
subset(x, subset, ...)

# S4 method for class 'itemsets'
subset(x, subset, ...)

# S4 method for class 'rules'
subset(x, subset, ...)
```

## Arguments

- x:

  object to be subsetted.

- ...:

  further arguments to be passed to or from other methods.

- subset:

  logical expression indicating elements to keep.

## Value

An object of the same class as `x` containing only the elements which
satisfy the conditions.

## Details

`subset()` finds the rows/itemsets/rules of `x` that match the
expression given in `subset`. Parts of `x` like items, lhs, rhs and the
columns in the quality data.frame (e.g., support and lift) can be
directly referred to by their names in `subset`.

Important operators to select itemsets containing items specified by
their labels are

- [%in%](http://michael.hahsler.net/arules/reference/match.md): select
  itemsets matching *any* given item

- [%ain%](http://michael.hahsler.net/arules/reference/match.md): select
  only itemsets matching *all* given item

- [%oin%](http://michael.hahsler.net/arules/reference/match.md): select
  only itemsets matching *only* the given item

- [%pin%](http://michael.hahsler.net/arules/reference/match.md): `%in%`
  with *partial matching*

## Author

Michael Hahsler

## Examples

``` r
data("Adult")
rules <- apriori(Adult)
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
#> Absolute minimum support count: 4884 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [31 item(s)] done [0.00s].
#> creating transaction tree ... done [0.02s].
#> checking subsets of size 1 2 3 4 5 6 7 8 9 done [0.07s].
#> writing ... [6137 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.01s].

## select all rules with item "marital-status=Never-married" in
## the right-hand-side and lift > 2
rules.sub <- subset(rules, subset = rhs %in% "marital-status=Never-married" &
  lift > 2)

## use partial matching for all items corresponding to the variable
## "marital-status"
rules.sub <- subset(rules, subset = rhs %pin% "marital-status=")

## select only rules with items "age=Young" and "workclass=Private" in
## the left-hand-side
rules.sub <- subset(rules, subset = lhs %ain%
  c("age=Young", "workclass=Private"))
```
