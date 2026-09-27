# Cross-tabulate joint occurrences across pairs of items

Provides the generic function `crossTable()` and a method to
cross-tabulate joint occurrences across all pairs of items.

## Usage

``` r
crossTable(x, ...)

# S4 method for class 'itemMatrix'
crossTable(
  x,
  measure = c("count", "support", "probability", "lift"),
  sort = FALSE
)
```

## Arguments

- x:

  object to be cross-tabulated
  ([transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)).

- ...:

  additional arguments.

- measure:

  measure to return. Default is co-occurrence counts.

- sort:

  sort the items by support.

## Value

A symmetric matrix of n x n, where n is the number of items times in
`x`. The matrix contains the co-occurrence counts between pairs of
items.

## See also

Other itemMatrix and transactions functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
[`image`](http://michael.hahsler.net/arules/reference/image.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md),
[`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md),
[`itemMatrix-class`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
[`itemwiseSetOps`](http://michael.hahsler.net/arules/reference/itemwiseSetOps.md),
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
data("Groceries")

ct <- crossTable(Groceries, sort = TRUE)
ct[1:5, 1:5]
#>                  whole milk other vegetables rolls/buns soda yogurt
#> whole milk             2513              736        557  394    551
#> other vegetables        736             1903        419  322    427
#> rolls/buns              557              419       1809  377    338
#> soda                    394              322        377 1715    269
#> yogurt                  551              427        338  269   1372

sp <- crossTable(Groceries, measure = "support", sort = TRUE)
sp[1:5, 1:5]
#>                  whole milk other vegetables rolls/buns       soda     yogurt
#> whole milk       0.25551601       0.07483477 0.05663447 0.04006101 0.05602440
#> other vegetables 0.07483477       0.19349263 0.04260295 0.03274021 0.04341637
#> rolls/buns       0.05663447       0.04260295 0.18393493 0.03833249 0.03436706
#> soda             0.04006101       0.03274021 0.03833249 0.17437722 0.02735130
#> yogurt           0.05602440       0.04341637 0.03436706 0.02735130 0.13950178

lift <- crossTable(Groceries, measure = "lift", sort = TRUE)
lift[1:5, 1:5]
#>                  whole milk other vegetables rolls/buns      soda   yogurt
#> whole milk               NA        1.5136341   1.205032 0.8991124 1.571735
#> other vegetables  1.5136341               NA   1.197047 0.9703476 1.608457
#> rolls/buns        1.2050318        1.1970465         NA 1.1951242 1.339363
#> soda              0.8991124        0.9703476   1.195124        NA 1.124368
#> yogurt            1.5717351        1.6084566   1.339363 1.1243678       NA
```
