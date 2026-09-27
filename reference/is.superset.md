# Find Super and Subsets

Provides the generic functions `is.subset()` and `is.superset()`, and
the methods for finding super or subsets in
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
and
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
objects.

## Usage

``` r
is.superset(x, y = NULL, proper = FALSE, sparse = TRUE, ...)

is.subset(x, y = NULL, proper = FALSE, sparse = TRUE, ...)

# S4 method for class 'itemMatrix'
is.superset(x, y = NULL, proper = FALSE, sparse = TRUE)

# S4 method for class 'associations'
is.superset(x, y = NULL, proper = FALSE, sparse = TRUE)

# S4 method for class 'itemMatrix'
is.subset(x, y = NULL, proper = FALSE, sparse = TRUE)

# S4 method for class 'associations'
is.subset(x, y = NULL, proper = FALSE, sparse = TRUE)
```

## Arguments

- x, y:

  associations or itemMatrix objects. If `y = NULL`, the super or subset
  structure within set `x` is calculated.

- proper:

  a logical indicating if all or just proper super or subsets.

- sparse:

  a logical indicating if a sparse
  [Matrix::ngCMatrix](https://rdrr.io/pkg/Matrix/man/nsparseMatrix-class.html)
  rather than a dense logical matrix should be returned. Sparse
  computation requires a significantly smaller amount of memory and is
  much faster for large sets.

- ...:

  currently unused.

## Value

returns a logical matrix or a sparse
[Matrix::ngCMatrix](https://rdrr.io/pkg/Matrix/man/nsparseMatrix-class.html)
with `length(x)` rows and `length(y)` columns. Each logical row vector
represents which elements in `y` are supersets (subsets) of the
corresponding element in `x`. If either `x` or `y` have length zero,
`NULL` is returned instead of a matrix.

## Details

Determines for each element in `x` which elements in `y` are supersets
or subsets. Note that the method can be very slow and memory intensive
if `x` and/or `y` are very dense (contain many items).

For rules, the union of lhs and rhs is used a the set of items.

## See also

Other postprocessing:
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)

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
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
[`image`](http://michael.hahsler.net/arules/reference/image.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
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

Michael Hahsler and Ian Johnson

## Examples

``` r
data("Adult")
set <- eclat(Adult, parameter = list(supp = 0.8))
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>     FALSE     0.8      1     10 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 39073 
#> 
#> create itemset ... 
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [4 item(s)] done [0.00s].
#> creating bit matrix ... [4 row(s), 48842 column(s)] done [0.00s].
#> writing  ... [8 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].

### find the supersets of each itemset in set
is.superset(set, set)
#> 8 x 8 sparse Matrix of class "ngCMatrix"
#>                                                  {race=White,capital-loss=None}
#> {race=White,capital-loss=None}                                                |
#> {capital-loss=None,native-country=United-States}                              .
#> {capital-gain=None,native-country=United-States}                              .
#> {capital-gain=None,capital-loss=None}                                         .
#> {capital-loss=None}                                                           .
#> {capital-gain=None}                                                           .
#> {native-country=United-States}                                                .
#> {race=White}                                                                  .
#>                                                  {capital-loss=None,native-country=United-States}
#> {race=White,capital-loss=None}                                                                  .
#> {capital-loss=None,native-country=United-States}                                                |
#> {capital-gain=None,native-country=United-States}                                                .
#> {capital-gain=None,capital-loss=None}                                                           .
#> {capital-loss=None}                                                                             .
#> {capital-gain=None}                                                                             .
#> {native-country=United-States}                                                                  .
#> {race=White}                                                                                    .
#>                                                  {capital-gain=None,native-country=United-States}
#> {race=White,capital-loss=None}                                                                  .
#> {capital-loss=None,native-country=United-States}                                                .
#> {capital-gain=None,native-country=United-States}                                                |
#> {capital-gain=None,capital-loss=None}                                                           .
#> {capital-loss=None}                                                                             .
#> {capital-gain=None}                                                                             .
#> {native-country=United-States}                                                                  .
#> {race=White}                                                                                    .
#>                                                  {capital-gain=None,capital-loss=None}
#> {race=White,capital-loss=None}                                                       .
#> {capital-loss=None,native-country=United-States}                                     .
#> {capital-gain=None,native-country=United-States}                                     .
#> {capital-gain=None,capital-loss=None}                                                |
#> {capital-loss=None}                                                                  .
#> {capital-gain=None}                                                                  .
#> {native-country=United-States}                                                       .
#> {race=White}                                                                         .
#>                                                  {capital-loss=None}
#> {race=White,capital-loss=None}                                     |
#> {capital-loss=None,native-country=United-States}                   |
#> {capital-gain=None,native-country=United-States}                   .
#> {capital-gain=None,capital-loss=None}                              |
#> {capital-loss=None}                                                |
#> {capital-gain=None}                                                .
#> {native-country=United-States}                                     .
#> {race=White}                                                       .
#>                                                  {capital-gain=None}
#> {race=White,capital-loss=None}                                     .
#> {capital-loss=None,native-country=United-States}                   .
#> {capital-gain=None,native-country=United-States}                   |
#> {capital-gain=None,capital-loss=None}                              |
#> {capital-loss=None}                                                .
#> {capital-gain=None}                                                |
#> {native-country=United-States}                                     .
#> {race=White}                                                       .
#>                                                  {native-country=United-States}
#> {race=White,capital-loss=None}                                                .
#> {capital-loss=None,native-country=United-States}                              |
#> {capital-gain=None,native-country=United-States}                              |
#> {capital-gain=None,capital-loss=None}                                         .
#> {capital-loss=None}                                                           .
#> {capital-gain=None}                                                           .
#> {native-country=United-States}                                                |
#> {race=White}                                                                  .
#>                                                  {race=White}
#> {race=White,capital-loss=None}                              |
#> {capital-loss=None,native-country=United-States}            .
#> {capital-gain=None,native-country=United-States}            .
#> {capital-gain=None,capital-loss=None}                       .
#> {capital-loss=None}                                         .
#> {capital-gain=None}                                         .
#> {native-country=United-States}                              .
#> {race=White}                                                |
is.superset(set, set, sparse = FALSE)
#>                                                  {race=White,capital-loss=None}
#> {race=White,capital-loss=None}                                             TRUE
#> {capital-loss=None,native-country=United-States}                          FALSE
#> {capital-gain=None,native-country=United-States}                          FALSE
#> {capital-gain=None,capital-loss=None}                                     FALSE
#> {capital-loss=None}                                                       FALSE
#> {capital-gain=None}                                                       FALSE
#> {native-country=United-States}                                            FALSE
#> {race=White}                                                              FALSE
#>                                                  {capital-loss=None,native-country=United-States}
#> {race=White,capital-loss=None}                                                              FALSE
#> {capital-loss=None,native-country=United-States}                                             TRUE
#> {capital-gain=None,native-country=United-States}                                            FALSE
#> {capital-gain=None,capital-loss=None}                                                       FALSE
#> {capital-loss=None}                                                                         FALSE
#> {capital-gain=None}                                                                         FALSE
#> {native-country=United-States}                                                              FALSE
#> {race=White}                                                                                FALSE
#>                                                  {capital-gain=None,native-country=United-States}
#> {race=White,capital-loss=None}                                                              FALSE
#> {capital-loss=None,native-country=United-States}                                            FALSE
#> {capital-gain=None,native-country=United-States}                                             TRUE
#> {capital-gain=None,capital-loss=None}                                                       FALSE
#> {capital-loss=None}                                                                         FALSE
#> {capital-gain=None}                                                                         FALSE
#> {native-country=United-States}                                                              FALSE
#> {race=White}                                                                                FALSE
#>                                                  {capital-gain=None,capital-loss=None}
#> {race=White,capital-loss=None}                                                   FALSE
#> {capital-loss=None,native-country=United-States}                                 FALSE
#> {capital-gain=None,native-country=United-States}                                 FALSE
#> {capital-gain=None,capital-loss=None}                                             TRUE
#> {capital-loss=None}                                                              FALSE
#> {capital-gain=None}                                                              FALSE
#> {native-country=United-States}                                                   FALSE
#> {race=White}                                                                     FALSE
#>                                                  {capital-loss=None}
#> {race=White,capital-loss=None}                                  TRUE
#> {capital-loss=None,native-country=United-States}                TRUE
#> {capital-gain=None,native-country=United-States}               FALSE
#> {capital-gain=None,capital-loss=None}                           TRUE
#> {capital-loss=None}                                             TRUE
#> {capital-gain=None}                                            FALSE
#> {native-country=United-States}                                 FALSE
#> {race=White}                                                   FALSE
#>                                                  {capital-gain=None}
#> {race=White,capital-loss=None}                                 FALSE
#> {capital-loss=None,native-country=United-States}               FALSE
#> {capital-gain=None,native-country=United-States}                TRUE
#> {capital-gain=None,capital-loss=None}                           TRUE
#> {capital-loss=None}                                            FALSE
#> {capital-gain=None}                                             TRUE
#> {native-country=United-States}                                 FALSE
#> {race=White}                                                   FALSE
#>                                                  {native-country=United-States}
#> {race=White,capital-loss=None}                                            FALSE
#> {capital-loss=None,native-country=United-States}                           TRUE
#> {capital-gain=None,native-country=United-States}                           TRUE
#> {capital-gain=None,capital-loss=None}                                     FALSE
#> {capital-loss=None}                                                       FALSE
#> {capital-gain=None}                                                       FALSE
#> {native-country=United-States}                                             TRUE
#> {race=White}                                                              FALSE
#>                                                  {race=White}
#> {race=White,capital-loss=None}                           TRUE
#> {capital-loss=None,native-country=United-States}        FALSE
#> {capital-gain=None,native-country=United-States}        FALSE
#> {capital-gain=None,capital-loss=None}                   FALSE
#> {capital-loss=None}                                     FALSE
#> {capital-gain=None}                                     FALSE
#> {native-country=United-States}                          FALSE
#> {race=White}                                             TRUE
```
