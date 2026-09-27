# Value Matching

Provides the generic function `match()` and the methods for
[associations](http://michael.hahsler.net/arules/reference/associations-class.md),
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
and
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
objects. `match()` returns a vector of the positions of (first) matches
of its first argument in its second.

## Usage

``` r
match(x, table, nomatch = NA_integer_, incomparables = NULL)

# S4 method for class 'itemMatrix,itemMatrix'
match(x, table, nomatch = NA_integer_, incomparables = NULL)

# S4 method for class 'rules,rules'
match(x, table, nomatch = NA_integer_, incomparables = NULL)

# S4 method for class 'itemsets,itemsets'
match(x, table, nomatch = NA_integer_, incomparables = NULL)

# S4 method for class 'itemMatrix,itemMatrix'
x %in% table

# S4 method for class 'itemMatrix,character'
x %in% table

# S4 method for class 'associations,associations'
x %in% table

# S4 method for class 'itemMatrix,character'
x %pin% table

# S4 method for class 'itemMatrix,character'
x %ain% table

# S4 method for class 'itemMatrix,character'
x %oin% table
```

## Arguments

- x:

  an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md).

- table:

  a set of associations or transactions to be matched against.

- nomatch:

  the value to be returned in the case when no match is found.

- incomparables:

  a logical; If `TRUE` then match recodes incompatible item orders
  quietly. Otherwise, recoding will create a warning.

## Value

`match`: An integer vector of the same length as `x` giving the position
in `table` of the first match if there is a match, otherwise `nomatch`.

`%in%`, `%pin%`, `%ain%`, `%oin%`: A logical vector, indicating if a
match was located for each element of `x`.

## Details

`%in%` is a more intuitive interface as a binary operator, which returns
a logical vector indicating if there is a match or not for the items in
the itemsets (left operand) with the items in the table (right operand).

arules defines additional binary operators for matching itemsets:
`%pin%` uses *partial matching* on the table; `%ain%` itemsets have to
match/include *all* items in the table; `%oin%` itemsets can *only*
match/include the items in the table. The binary matching operators or
often used in
[`subset()`](http://michael.hahsler.net/arules/reference/subset.md).

## See also

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

## get unique transactions, count frequency of unique transactions
## and plot frequency of unique transactions
vals <- unique(Adult)
cnts <- tabulate(match(Adult, vals))
plot(sort(cnts, decreasing = TRUE))


## find all transactions which are equal to transaction 10 in Adult
which(Adult %in% Adult[10])
#> [1]    10  8438  8595 12023 12752 24123 24455 28063

## for transactions we can also match directly with itemLabels.
## Find in the first 10 transactions the ones which
## contain age=Middle-aged (see help page for class itemMatrix)
Adult[1:10] %in% "age=Middle-aged"
#>  [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE

## find all transactions which contain items that partially match "age=" (all here).
Adult[1:10] %pin% "age="
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

## find all transactions that only include the item "age=Middle-aged" (none here).
Adult[1:10] %oin% "age=Middle-aged"
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

## find al transaction which contain both items "age=Middle-aged" and "sex=Male"
Adult[1:10] %ain% c("age=Middle-aged", "sex=Male")
#>  [1]  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
```
