# Support Counting for Itemsets

Counts support for itemsets represented by an
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
or an
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
object in a
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
data set.

## Usage

``` r
support(x, transactions, ...)

# S4 method for class 'itemMatrix'
support(
  x,
  transactions,
  type = c("relative", "absolute"),
  method = c("ptree", "tidlists"),
  reduce = FALSE,
  weighted = FALSE,
  verbose = FALSE,
  ...
)

# S4 method for class 'associations'
support(
  x,
  transactions,
  type = c("relative", "absolute"),
  method = c("ptree", "tidlists"),
  reduce = FALSE,
  weighted = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  or
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md)
  object containing the itemsets for which support is counted.

- transactions:

  the
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  data set in which support is counted.

- ...:

  further arguments passed from the generic to a method.

- type:

  return `"relative"` support or `"absolute"` counts (or summed weights
  when `weighted = TRUE`).

- method:

  support-counting method: `"ptree"` or `"tidlists"`.

- reduce:

  logical; remove unused items before prefix-tree counting?

- weighted:

  logical; use transaction weights stored in the `weight` column of
  [`transactionInfo()`](http://michael.hahsler.net/arules/reference/transactions-class.md)?

- verbose:

  logical; report progress and timing information?

## Value

An unnamed numeric vector of length `length(x)`. Values are relative
supports when `type = "relative"` and counts or weight sums when
`type = "absolute"`.

## Details

Normally, the support of frequent itemsets is counted efficiently during
the mining process using a minimum support threshold. However, if only
the support for specific itemsets (maybe itemsets with very low support)
is needed, or the support of a set of itemsets needs to be recalculated
on different
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
than they were mined on, then `support()` can be used.

Several methods for support counting are available:

- `"ptree"` (default method): The counters for the itemsets are
  organized in a prefix tree. The transactions are sequentially
  processed and the corresponding counters in the prefix tree are
  incremented (see Hahsler et al, 2008). This method is used by default
  since it is typically significantly faster than transaction ID list
  intersection.

- `"tidlists"`: Support is counted using transaction ID list
  intersection which is used by several fast mining algorithms (e.g., by
  Eclat). However, support is determined for each itemset individually
  which is slow for a large number of long itemsets in dense data.

The item coding of `x` and `transactions` is reconciled using item
labels. Items that occur only in `transactions` do not affect the count.
With `reduce = TRUE`, unused items are removed before prefix-tree
counting.

Weighted support uses the numeric `weight` column in
`transactionInfo(transactions)`. Absolute weighted support is the sum of
the weights of supporting transactions; relative weighted support
divides this value by the sum of all transaction weights.

## References

Michael Hahsler, Christian Buchta, and Kurt Hornik. Selective
association rule generation. *Computational Statistics*, 23(2):303-315,
April 2008.

## See also

Other interest measures:
[`confint`](http://michael.hahsler.net/arules/reference/confint.md),
[`coverage()`](http://michael.hahsler.net/arules/reference/coverage.md),
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md)

## Author

Michael Hahsler and Christian Buchta

## Examples

``` r
data("Income")

## find and some frequent itemsets
itemsets <- eclat(Income)[1:5]
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>     FALSE     0.1      1     10 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 687 
#> 
#> create itemset ... 
#> set transactions ...[50 item(s), 6876 transaction(s)] done [0.00s].
#> sorting and recoding items ... [30 item(s)] done [0.00s].
#> creating bit matrix ... [30 row(s), 6876 column(s)] done [0.00s].
#> writing  ... [5571 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].

## inspect the support returned by eclat
inspect(itemsets)
#>     items                              support count
#> [1] {occupation=clerical/service,                   
#>      language in home=english}       0.1127109   775
#> [2] {education=no college graduate,                 
#>      ethnic classification=hispanic} 0.1096568   754
#> [3] {marital status=married,                        
#>      dual incomes=no,                               
#>      householder status=own,                        
#>      language in home=english}       0.1007853   693
#> [4] {dual incomes=no,                               
#>      householder status=own,                        
#>      language in home=english}       0.1019488   701
#> [5] {marital status=married,                        
#>      dual incomes=no,                               
#>      householder status=own}         0.1058755   728

## count support in the database
support(items(itemsets), Income)
#> [1] 0.1127109 0.1096568 0.1007853 0.1019488 0.1058755
support(itemsets, Income, type = "absolute")
#> [1] 775 754 693 701 728
```
