# Set Operations

Provides the generic functions and the methods for the set operations
[`union()`](https://rdrr.io/r/base/sets.html),
[`intersect()`](https://rdrr.io/r/base/sets.html),
[`setequal()`](https://rdrr.io/r/base/sets.html),
[`setdiff()`](https://rdrr.io/r/base/sets.html) and
[`is.element()`](https://rdrr.io/r/base/sets.html) on sets of
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
(e.g.,
[rules](http://michael.hahsler.net/arules/reference/rules-class.md),
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md))
and
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

## Usage

``` r
# S3 method for class 'itemMatrix'
union(x, y, ...)

# S3 method for class 'associations'
union(x, y, ...)

# S4 method for class 'associations'
union(x, y, ...)

# S4 method for class 'itemMatrix'
union(x, y, ...)

# S3 method for class 'itemMatrix'
intersect(x, y, ...)

# S3 method for class 'associations'
intersect(x, y, ...)

# S4 method for class 'associations'
intersect(x, y, ...)

# S4 method for class 'itemMatrix'
intersect(x, y, ...)

# S3 method for class 'itemMatrix'
setequal(x, y, ...)

# S3 method for class 'associations'
setequal(x, y, ...)

# S4 method for class 'associations'
setequal(x, y, ...)

# S4 method for class 'itemMatrix'
setequal(x, y, ...)

# S3 method for class 'itemMatrix'
setdiff(x, y, ...)

# S3 method for class 'associations'
setdiff(x, y, ...)

# S4 method for class 'associations'
setdiff(x, y, ...)

# S4 method for class 'itemMatrix'
setdiff(x, y, ...)

# S3 method for class 'itemMatrix'
is.element(el, set, ...)

# S3 method for class 'associations'
is.element(el, set, ...)

# S4 method for class 'associations'
is.element(el, set, ...)

# S4 method for class 'itemMatrix'
is.element(el, set, ...)
```

## Arguments

- x, y, el, set:

  sets of associations or itemMatrix objects.

- ...:

  Other arguments are unused.

## Value

[`union()`](https://rdrr.io/r/base/sets.html),
[`intersect()`](https://rdrr.io/r/base/sets.html),
[`setequal()`](https://rdrr.io/r/base/sets.html) and
[`setdiff()`](https://rdrr.io/r/base/sets.html) return an object of the
same class as `x` and `y`.

[`is.element()`](https://rdrr.io/r/base/sets.html) returns a logic
vector of length `el` indicating for each element if it is included in
`set`.

## Details

Technical note: All S4 methods for set operations are defined for the
class name `"ANY"` in the signature, so they should work for all S4
classes for which the following methods are available:
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`length()`](https://rdrr.io/r/base/length.html) and
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md).

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
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`rules-class`](http://michael.hahsler.net/arules/reference/rules-class.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
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
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`random.transactions()`](http://michael.hahsler.net/arules/reference/random.transactions.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
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

## mine some rules
r <- apriori(Adult)
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

## take 2 subsets
r1 <- r[1:10]
r2 <- r[6:15]

union(r1, r2)
#> set of 15 rules 
intersect(r1, r2)
#> set of 5 rules 
setequal(r1, r2)
#> [1] FALSE
```
