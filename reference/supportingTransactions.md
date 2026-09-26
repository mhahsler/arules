# Supporting Transactions

Find for each itemset in an
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
object which transactions support (i.e., contains all items in the
itemset) it. The information is returned as a
[tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md)
object.

## Usage

``` r
supportingTransactions(x, transactions, ...)

# S4 method for class 'associations'
supportingTransactions(x, transactions)
```

## Arguments

- x:

  a set of
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md)
  ([itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md),
  [rules](http://michael.hahsler.net/arules/reference/rules-class.md),
  etc.)

- transactions:

  an object of class
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  used to mine the associations in `x`.

- ...:

  currently unused.

## Value

An object of class
[tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md)
containing one transaction ID list per association in `x`.

## See also

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
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`random.transactions()`](http://michael.hahsler.net/arules/reference/random.transactions.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`tidLists-class`](http://michael.hahsler.net/arules/reference/tidLists-class.md),
[`transactions-class`](http://michael.hahsler.net/arules/reference/transactions-class.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
data <- list(
  c("a", "b", "c"),
  c("a", "b"),
  c("a", "b", "d"),
  c("b", "e"),
  c("b", "c", "e"),
  c("a", "d", "e"),
  c("a", "c"),
  c("a", "b", "d"),
  c("c", "e"),
  c("a", "b", "d", "e")
)
data <- as(data, "transactions")

## mine itemsets
f <- eclat(data, parameter = list(support = .2, minlen = 3))
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>     FALSE     0.2      3     10 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 2 
#> 
#> create itemset ... 
#> set transactions ...[5 item(s), 10 transaction(s)] done [0.00s].
#> sorting and recoding items ... [5 item(s)] done [0.00s].
#> creating bit matrix ... [5 row(s), 10 column(s)] done [0.00s].
#> writing  ... [2 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].
inspect(f)
#>     items     support count
#> [1] {a, d, e} 0.2     2    
#> [2] {a, b, d} 0.3     3    

## find supporting Transactions
st <- supportingTransactions(f, data)
st
#> tidLists in sparse format with
#>  2 items/itemsets (rows) and
#>  10 transactions (columns)

as(st, "list")
#> $`{a,d,e}`
#> [1]  6 10
#> 
#> $`{a,b,d}`
#> [1]  3  8 10
#> 
```
