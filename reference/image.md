# Visual Inspection of Binary Incidence Matrices

Provides `image()` methods to generate level plots to visually inspect
binary incidence matrices, i.e., objects based on
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
(e.g.,
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md),
[tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md),
items in
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
or rhs/lhs in
[rules](http://michael.hahsler.net/arules/reference/rules-class.md)).
These plots can be used to identify problems in a data set (e.g.,
recording problems with some transactions containing all items).

## Usage

``` r
# S4 method for class 'itemMatrix'
image(x, xlab = "Items (Columns)", ylab = "Elements (Rows)", ...)

# S4 method for class 'transactions'
image(x, xlab = "Items (Columns)", ylab = "Transactions (Rows)", ...)

# S4 method for class 'tidLists'
image(x, xlab = "Transactions (Columns)", ylab = "Items/itemsets (Rows)", ...)
```

## Arguments

- x:

  the object
  ([itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or
  [tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md)).

- xlab, ylab:

  labels for the plot.

- ...:

  further arguments passed on to `image()` in package Matrix which in
  turn are passed on to `levelplot()` in lattice.

## See also

`image()` in package Matrix

Other itemMatrix and transactions functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
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
data("Epub")

## in this data set we can see that not all
## items were available from the beginning.
image(Epub[1:1000])
```
