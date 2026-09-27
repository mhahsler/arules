# Methods for "\[": Extraction or Subsetting arules Objects

Methods for `"["`, i.e., extraction or subsetting for arules objects.

## Usage

``` r
# S4 method for class 'itemMatrix,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'transactions,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'tidLists,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'rules,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'itemsets,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md).

- i:

  select rows/sets using an integer vector containing row numbers or a
  logical vector.

- j:

  select columns/items using an integer vector containing column numbers
  (i.e., item IDs), a logical vector or a vector of strings containing
  parts of item labels.

- ...:

  further arguments are ignored.

- drop:

  ignored.

## See also

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
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
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
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
data(Adult)
Adult
#> transactions in sparse format with
#>  48842 transactions (rows) and
#>  115 items (columns)

## select first 10 transactions
Adult[1:10]
#> transactions in sparse format with
#>  10 transactions (rows) and
#>  115 items (columns)

## select first 10 items for first 100 transactions
Adult[1:100, 1:10]
#> transactions in sparse format with
#>  100 transactions (rows) and
#>  10 items (columns)

## select the first 100 transactions for the items containing
## "income" or "age=Young" in their labels
Adult[1:100, c("income=small", "income=large", "age=Young")]
#> transactions in sparse format with
#>  100 transactions (rows) and
#>  3 items (columns)
```
